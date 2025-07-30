

/*
The basic goal of these classes is to create a patching environment akin to that of an analog synthesizer. Bus mapping is how patching is implemented.  Interestingly, mapping allows control and audio rate modulation to be interoperable.

Since the term 'map' is used both for parameter scaling and  bus mapping, this library
uses the term 'patch' for bus mapping

MODULE
A Module consists of

!. a SynthDef which uses distinct named Controls for assigning each of its outputs to a bus. (Those names are  also used to identify the bus for patching.)

2. A declaration of which of the SynthDef's controls are patchable modulation inputs
and how many parallel sources of modulation each supports.

PATCHPOINT
A PatchPoint sets the value of one of the Controls of a Modyule's synth.
It consists of
	The ControlName object taken from a SynthDesc to determine, name, rate and default value
	A bus mapped to a control on a Module's synth
	An array of "inlet synths" that provide modulation inputs with depth controls
	A "scaling synth " that sets the control's initial value, combines that with the modulation inputs
and scales the result from a nominal [0,1] range to the range of the Module synth's control.

Notes:
ControlSpec determination employ the usual mapping from name to a ControlSpec with some added logic. The search for the ControlSpec is based on the ControlName reduced to alphabetic characters
That way, 'freq1' or 'freq 1' or 'freq_1' will all be assigned \freq.asSpec

Module has its own dictionary of ControlSpecs is consulted first, in an attempt to avoid polluting the main ControlSpec namespace

Also the Warp of the ControlSpec can be used to select a different  version of the scaling synth.  This is used by FaderWarp to treat the initial value as a masterfader.  (So, instead of adding the initial value to the contents of the modulation bus, it scales the modulation bus and the modulation sources are effectively mixer inputs.)



*/

/*
patch saving, restoring, and updating
update - value is a string moduleName + outputName

*/
/* Module uses the Warp of a PatchPoint's ControlSpec to identify its role as:

"scaled" - PatchPoint inlets are summed and scaled to the controls range.  Inlet 0 sets an initial value and is not patchable
"summed" - PatchPoint mixes audio inputs for processing
"discrete"  - PatchPoint inlets are triggers or gates
*/

DiscreteWarp : Warp {
	map { arg value;
		// maps a value from [0..1] to spec range
		^value.round(1)
	}
	unmap { arg value;
		// maps a value from spec range to [0..1]
		^value
	}
}

PatchPoint {

	classvar <scalerRateDict;	// dictionary for audio and control rate of inlet and scaling SynthDefs
	var <moduleName;
	var <name;
	var <rate;					// audio or control rate
	var  <numInlets;			// number of modulation inputs
	var <spec;					// spec for the Control
	var <>type;				// scaled or summed or gate - FaderWarp specs are summed, 'discrete'  are gates
	// var <size;			   // future: size for managing multichannel Controls
	var <bus;					// bus mapped to the Module control
	var <value;					// the initial value of the control, here as a potentia convenience

	var <>inletSynths;		// array of synths that provide modulation inputs for this patchPoint
										// its first element is a "scaling synth" that maps the bus value to the Control's range
										// the rest are instances of inlet_audio and inlet_control
	var <scalerDefName;	 // alternate defs for scaling  that implement different Warps
												//  special defs can be selected by Warp...FaderWarp uses initialValue as a master fader
	var <specArray;			// spec as an argument array used by the scalingSynth

	var <stateDict;			// dictionary keyed by inlet index for values and index as a Symbol for patch connections

	var <>changed;			// dictionary of changed values, update adds these values to stateDict
	var <>links;	// dictionary of objects that need to be updated when patchpoint stateDict changes

	*initClass {
		// Class.initClassTree(ControlSpec);
		// Class.initClassTree(Server);
		StartUp.add({

			Warp.warps[\discrete] = DiscreteWarp;
			SynthDef("inlet_audio", { Out.ar(\bus.kr, \modSource.ar * \cv.kr(0.0)); }).add;
			SynthDef("inlet_control", { Out.kr(\bus.kr, \modSource.kr * \cv.kr(0.0)); }).add;
			SynthDef("gateInlet_audio", { Out.kr(\bus.kr, \modSource.ar ); \cv.kr}).add;
			SynthDef("gateInlet_control", { Out.kr(\bus.kr, \modSource.kr + \cv.kr(0) )}).add;

			SynthDef("clear_ControlBus",  { Out.kr(\bus.kr, 0); }).add;

			// SynthDef("faderScaler_audio", {|bus, cv = 0|
			// 	// var val = MulAdd(InFeedback.ar(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
			// 	var val = InFeedback.ar(bus);
			// 	val = val  * cv.squared;
			// 	ReplaceOut.ar(bus, val);
			// }).add;

			SynthDef("discreteScaler_audio", {|bus, cv = 0, minval = 0, maxval = 1, step = 0|
				// var val = MulAdd(InFeedback.ar(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
				var val = InFeedback.ar(bus);
				ReplaceOut.ar(bus, val);
			}).add;

		SynthDef("discreteScaler_control", {|bus, cv = 0, minval = 0, maxval = 1, step = 0|
				// var val = MulAdd(InFeedback.ar(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
				var val = In.kr(bus);
				ReplaceOut.kr(bus, val);
			}).add;

			SynthDef("linScaler_audio", {|bus, cv = 0, minval = 0, maxval = 1, step = 0|
				// var val = MulAdd(InFeedback.ar(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
				var val = InFeedback.ar(bus);
				val = val + cv;
				// val = (val * (maxval - minval) + minval).round(step);
				val = val.linlin(0, 1, minval, maxval).round(step);
				ReplaceOut.ar(bus, val);
			}).add;

			SynthDef("expScaler_audio", {|bus, cv = 0, minval = 0.00001, maxval = 1,step = 0|
				// var val = MulAdd(InFeedback.ar(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
				var val = InFeedback.ar(bus);
				val = val + cv;
				// val =  pow(maxval/minval, val) * minval;
				val = val.linexp(0, 1, minval, maxval).round(step);
				ReplaceOut.ar(bus, val);
			}).add;

			SynthDef("curveScaler_audio", {|bus, cv = 0, minval = 0.000, maxval = 1,  curve = 1, step = 0 |
				// var val = MulAdd(InFeedback.ar(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
				var grow, a, b;
				var val = InFeedback.ar(bus);
				val = val + cv;
				// curve = max(0.001, curve.abs) * curve.sign;
				// grow = exp(curve);
				// a = maxval - minval / (1.0 - grow);
				// b =  minval + a;
				// val = b - (a * pow(grow, val));
				val = val.lincurve(0, 1, minval, maxval, curve).round(step);
				ReplaceOut.ar(bus, val);
			}).add;

			SynthDef("faderScaler_control", {|bus, cv = 0|
				// var val = MulAdd(In.kr(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
				var val = In.kr(bus);
				val = val  * cv.squared;
				ReplaceOut.kr(bus, val);
			}).add;

			SynthDef("linScaler_control", {|bus, cv = 0, minval = 0, maxval = 1, step = 0|
				// var val = MulAdd(InFeedback.ar(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
				var val = In.kr(bus);
				val = val + cv;
				// val = (val * (maxval - minval) + minval).round(step);
				val = val.linlin(0, 1, minval, maxval).round(step);
				ReplaceOut.kr(bus, val);
			}).add;

			SynthDef("expScaler_control", {|bus, cv = 0, minval = 0.00001, maxval = 1, step = 0|
				// var val = MulAdd(InFeedback.ar(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
				var val = In.kr(bus);
				val = val + cv;
				// val =  pow(maxval/minval, val) * minval;
				val = val.linexp(0, 1, minval, maxval).round(step);
				ReplaceOut.kr(bus, val);
			}).add;

			SynthDef("curveScaler_control", {|bus, cv = 0, minval = 0.000, maxval = 1,  curve = 1, step = 0 |
				// var val = MulAdd(In.kr(bus),0.5, 0.5).linlin(minval, maxval, 0, 1);
				var grow, a, b;
				var val = In.kr(bus);
				val = val + cv;
				// curve = max(0.001, curve.abs) * curve.sign;
				// grow = exp(curve);
				// a = maxval - minval / (1.0 - grow);
				// b =  minval + a;
				// val = b - (a * pow(grow, val));
				val = val.lincurve(0, 1, minval, maxval, curve).round(step);
				ReplaceOut.kr(bus, val);
			}).add;
			scalerRateDict =
			Dictionary.new.putPairs([
				DiscreteWarp, "discreteScaler",
				FaderWarp, "faderScaler",
				LinearWarp, "linScaler",
				ExponentialWarp, "expScaler",
				CurveWarp,  "curveScaler",
			])
		});
	}

	*new { | moduleName, controlName, numInlets = 2 |
		^super.newCopyArgs(moduleName, controlName.name, controlName.rate).init (numInlets);
	}

	init { | inletCount |
		links = IdentityDictionary.new;
		stateDict = ();
		this.numInlets_(inletCount);
		changed = Set.new;
		this.spec_(name);
	}

	numInlets_ { | num |
		numInlets = num;
		(numInlets-1).do { |i |
			stateDict[i + 1] = 0;
			stateDict[(i+1).asSymbol] = [\nc, ""];
		};
		if (type == \summed || (type ==\discrete)) { stateDict[\0] = [\nc, ""] };

	}

	update {
		changed.asArray.do { | key |
			var val = stateDict[key];
			if (key.class == Symbol) {
			if (key == \value) {
					links[\value].do { |dep | dep.value_(value) };
				} {
					links[key].do { |dep |
						if (val[0].asSymbol != \nc) {
							dep.string_((val[0].asString + val[1]))
						} {
							dep.string_(name.asString + key)
						}
					}
				}
			} {
				links[key].do{ | dep | dep.value = val; };
			};
		};
		changed.clear;
	}

	spec_ { | specSpec = \unipolar |
		var nm, warp;
		// find the name of the spec by removing all non-Alpha characters
		// that way, \freq1, \freq_1, etc are all scaled by \freq.asSpec
		if(specSpec.class != Array && (specSpec.class != Nil) ) {
			nm= specSpec.asString.select(_.isAlpha).asSymbol;
			spec = Module.specs[nm] ? nm.asSpec ? \unipolar.asSpec;
		} {
			spec = specSpec.asSpec ;		// here for [minval, maxval, warp, step, default] specs...
		};
		// now we use the warp of the spec to select which scaling synthdef to use
		warp = spec.warp;
		specArray = [minval: spec.minval, maxval: spec.maxval];
		scalerDefName = PatchPoint.scalerRateDict[warp.class] ++  "_" ++ rate;
		type = \scaled;
		case
			{warp.isKindOf(CurveWarp)} { specArray = specArray ++ [curve: warp] }
			{warp.isKindOf(FaderWarp)} { type = \summed  }
			{warp.isKindOf(DiscreteWarp)} { type = \discrete  };
	/*	if(warp.isKindOf(CurveWarp) ) { specArray = specArray ++ [curve: warp] };
		if (warp.isKindOf(FaderWarp) ) { type = \summed } { type = \scaled};
*/
		this.value_(spec.default)
	}

	value_ { | v |
		value = v;
		this.cv_(0, spec.unmap(v));
	}

	input_ { | v |
		this.cv_(0, v);
	}

	input { ^stateDict[0] }

		/*
	PatchPoints need to distinguish "mixer" and a"CV" modes
	In mixer mode, all inputs are patchable with input levels  scaled according to the spec
	In CV mode,  inputs are scaled linearly, the  scaling synth overwrites the bus with a scaled version and
	its control is used to set an initial value of the parameter.
	This could have different GUI representations (perhaps knobs vs sliders), so it is worth having a flag.
	This can be implemented by making scalerDefName nil....

	*/
	cv_ { | index, val |
		stateDict[index] = val;
		changed.add(index);

		if( index == 0 && (type == \scaled) ) {
				value = spec.map(val);
				stateDict[\value] = value;
				changed.add(\value);
		};
		inletSynths !? { inletSynths[index] !? { |inSyn | inSyn.set(\cv, val) }  };

	}

	patch { | index, sourceModuleName, sourceOutputName |
		var srcM, srcbus, indSymb;
		// hackish solution : index as Symbol indicates a patch, as Number a cv value.
		indSymb = index.asSymbol;
		stateDict[indSymb] = [sourceModuleName, sourceOutputName];
		changed.add(indSymb);
		if (sourceModuleName == \nc || (sourceModuleName == nil) ) {		// special case for no connection - set to 0 rather than map
			inletSynths !? { inletSynths[index] !?  { | inSyn | inSyn.set(\modSource, 0) } };
		} {
			srcM = Module.all[sourceModuleName] ?? {^nil};					// fail silently if the source module DNE
			srcbus = srcM.outputBuses[sourceOutputName] ?? {^nil};	// ditto for output
			inletSynths[index] !? { | inSyn |												// now, if its running, do the mapping
				inSyn.map(\modSource, srcbus);
			}
		}
	}

	state { ^stateDict.getPairs }		// runtime dictionary becomes a kv array for storage

	state_ { | state |
			stateDict.putPairs(state);
		(numInlets).do { | i |
			this.cv_(i, stateDict[i]);
			this.patch(i,*stateDict[i.asSymbol]);
		};
		this.update;
	}

	play { | group, synth|
		var inletcount, scalingSynth, inletSynthDefName = "inlet_" ++ rate;
		bus = Bus.perform(rate,  group.server, 1);
		synth.map(name, bus);										// connect patchPoint's bus to module synth control
		inletSynths = [];
	//	if(inletcount ==1 && (rate ==\control)) { Synth("clear_ControlBus", [bus: bus], group, \addToHead) };
		case
		{ type == \discrete }
		{
			inletSynthDefName = "gateInlet_" ++ rate;
			inletSynths = numInlets.collect { | i |
				Synth(inletSynthDefName, [bus: bus], group, \addToHead);
			};
				Synth("clear_ControlBus", [bus: bus], group, \addToHead);
		}
		{type ==\scaled}
		{
			scalingSynth = Synth(scalerDefName,  ([bus: bus, cv: stateDict[0]] ++ specArray), group, \addToHead);
			inletcount = numInlets -1;
			if ((inletcount = numInlets -1) == 0  && (rate == \control)) {
				Synth("clear_ControlBus", [bus: bus], group, \addToHead);
			} {
				inletSynths = inletcount.collect { | i |
					Synth(inletSynthDefName, [bus: bus, cv: stateDict[i + 1]], group, \addToHead);
				}
			};
			inletSynths = inletSynths.addFirst(scalingSynth);
		}
		{ type ==\summed }
		{
			inletSynths = numInlets.collect { | i |
				Synth(inletSynthDefName, [bus: bus, cv: stateDict[i ]], group, \addToHead);
			}
		}
	}

	free {  inletSynths = [];  bus.free; bus = nil; }

	/*
	note: each patchPoint creates and frees its own bus but Module frees the group containing all the inletSynths.
Module also frees output buses.

	*/

}

 Module {
	classvar <all;
	classvar <>orderedAll;
	classvar <>specs;
	classvar<>states;

	var <name;
	var <synthDefName;
	var <>group, <synth;
	var <inputNames;			// control Names
	var <patchPoints;			// dictionary inputname -> PatchPoint
	var <outputIODescs;		// we don't make a separate object for outputs, so we hold on to the
											// synthdesc output descriptor for rate and channel count information
	var <outputNames;		// we keep the names of outputs in an array for convenience for interface building
	var <outputBuses;			// dictionary of ouput name - > outBus
	// to implement something like a normaled connection

	*initClass {
		StartUp.add({
			Module.clear;
		});
	}

	*clear { 	all = IdentityDictionary.new; orderedAll = []; specs = IdentityDictionary.new; orderedAll  = nil;}

	*new {  | name, synthDefName, inletNumSpec |
		if (name.isNumber.not) {
			var res = all.at(name);
			if(res.isNil) {
				^super.newCopyArgs(name, synthDefName).init(inletNumSpec)
			} {
				^res
			}
		} {
			"broken module definition".error
		}
	}
	*addSpec { | name, spec| specs[name] = spec.asSpec }

	init { | inletNumSpec |
		var synthDesc, inputControlNames, namedControls;
		Module.all[name] = this;
		Module.orderedAll = Module.orderedAll .add(this);
			// these dictionaries are used for patching by name
		outputBuses = IdentityDictionary.new;
		patchPoints =  IdentityDictionary.new;

		synthDesc = SynthDescLib.global[synthDefName];
		namedControls = synthDesc.controls.select { | ctl| ctl.name.class == Symbol ; };
		outputIODescs = synthDesc.outputs.
			select({ | oCN | oCN.startingChannel.isKindOf(Symbol ) });
		outputNames = outputIODescs.collect(_.startingChannel);
		inputControlNames = namedControls.reject({ | ctl | outputNames.includes(ctl.name) });
		inputControlNames.do { | controlName |				//PatchPoint manages control vs audio rates, Module need not know
			patchPoints[controlName.name] = PatchPoint(name, controlName)
		};
		inputNames = inputControlNames.collect(_.name);			// this array controls the order of inputs in GUI display
	}

	setInletNums { | kvarray |
		kvarray.pairsDo { | name, num |
			patchPoints[name] !? { | pp | pp.numInlets_(num);  };
		};
	}

	makeOutputBuses { | server |
		outputIODescs.do { | ioDesc |
			outputBuses[ioDesc.startingChannel] = Bus.perform(ioDesc.rate, server, ioDesc.numberOfChannels);
		};
	}

	play { | target |
		var server;
		if (synthDefName.notNil) {
		target = target.asTarget;
		server = target.server;

		server.makeBundle(nil, {
			group = Group(target, \addToHead);
				NodeWatcher.register(group);
			synth = Synth(synthDefName, outputBuses.getPairs, group, \addToTail);
			patchPoints.do { | patchPoint |
				patchPoint.play(group, synth);
			};
		});
		patchPoints.do ({ | pp | pp.state_(pp.state);  });
		}
	}

	free {
		group.free;
		synth = nil; group = nil;
		patchPoints.do { | pp | pp.free };
	}

	state {
		^patchPoints.asArray.collect { | pp | [pp.name, pp.state] }.flatten ;
	}

	state_ { | state |
		state.pairsDo { | key, state |
				patchPoints[key] !? { | pp |
					pp.state_(state)
				}
		}
	}

	// initial value of control
	get { | name, v |
		^patchPoints[name].value
	}

	set { | name, v|
		patchPoints[name].value_(v)
	}

	input_ {  |name, v|
		patchPoints[name].input_(v);
	}

	value_ {  |name, v|
		patchPoints[name].value_(v);
	}

	cv_ {  |name, index, v|
		patchPoints[name].cv_(index, v);
	}

	patch { | inputName, index, sourceModuleName, sourceOutputName |
		patchPoints[inputName]  !?  { | pp |
			pp.patch(index, sourceModuleName, sourceOutputName)
		}
	}

	*patch { | inputModuleName, inputName, index, sourceModuleName, sourceOutputName |
		var inM,  patchPoint;
		inM = all[inputModuleName] ?? { ^nil };		// fail silently if the Module does not exist
		inM.patch( inputName, index, sourceModuleName, sourceOutputName);
	}

	*depth { | inputModuleName, inputName, index, depth |
		all[inputModuleName].patchPoints[inputName] !?
		{ | patchPoint | patchPoint.i.cv_(index, depth) }
	}

	*update {
		Module.orderedAll.do { |  m |
			m.patchPoints.do { |pp | pp.update }
		}
	}

	*updateSynths {
		Module.orderedAll.do { |  m |
			m.patchPoints.do { |pp | pp.updateSynths }
		}
	}

	*play { | target |
		var server =  target.asTarget.server;
		orderedAll.do({ | module | module.makeOutputBuses(server) });
		orderedAll.do({ | module | module.play(target) });
	}

	*free {
		orderedAll.do({ | module | module.free });
	}

	*state {
		^Module.orderedAll.collect { | m |
			[m.name, m.state];
		}.flatten;
	}

	*state_ { | state|
		state.pairsDo { | name, state |
			Module(name).state_(state)
		}
	}

	*addState { | name |
			Module.states = Module.states.add([name.asSymbol, Module.state]);
	}

	*setState { | name |
			var names, index;
			if (Module.states.size != 0) {
				names= Module.states.flop.first;
				 index = names.indexOf(name.asSymbol);
				if ( index.notNil ) {
					Module.state =  Module.states[index][1]
				}
			};
	}

	*clearStates {
		Module.states = [];
	}
}