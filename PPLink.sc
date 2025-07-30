MIDIMapper {
	var <>ccMappings;
	var ccMonitor;
	*new { ^super.new.init }

	init {
		MIDIClient.disposeClient;
		MIDIIn.connectAll;
		if(ccMappings.isNil) { ccMappings = () };
		if (ccMonitor.notNil) { ccMonitor.free };
		ccMonitor =	MIDIFunc.cc( { | val, cc, chan |
			var v, pp, index;
			if( PPGUI.midiCandidate.notNil)  {
				#v, pp, index = PPGUI.midiCandidate;
				PPGUI.midiCandidate = nil;
				defer { v.background = Color(0.918, 0.9183, 0.918) };
				ccMappings[cc] = [pp, index]
			} {
				if (ccMappings[cc].notNil) {
					#pp, index = ccMappings[cc];
					ccMappings[cc][0].cv_(ccMappings[cc][1], val/127)
				}
			};
		})
	}

	free { ccMonitor.free }
	clearMappings { ccMappings = () }

	getMappings  {
		^ccMappings.collect { | v , i | [v[0].moduleName, v[0].name, v[1],  i ]  };
	}

	setMappings { | mappings |
		var modName, ppName,mod, pp, index, cc;
		mappings.do { | m |
			#modName, ppName, index, cc = m;
			mod = Module(modName);
			if (mod.notNil) {
				pp = mod.patchPoints[ppName];
				ccMappings[cc] = [pp, index];
			}
		}
	}

	saveMIDIMappings { | path |
		var midiFolderPath;
		if (path.notNil) {
		} {
			midiFolderPath = Platform.userAppSupportDir +/+ "module midi maps";
			if (pathMatch(midiFolderPath).size == 0) {
				mkdir(midiFolderPath)
			};
		};
		Dialog.savePanel({ | path |
			var file = File(path,"wb");
			file.putString("\n");
			file.putString(this.getMappings.asCompileString ++ ";");
			file.putString("\n");
			file.close;
		}, path: path ? midiFolderPath);
	}

	loadMIDIMappings { | path |
		var midiFolderPath = Platform.userAppSupportDir +/+ "module midi maps";
		Dialog.openPanel({ arg path;
			this.setMappings(path.load);
		}, path: path ? midiFolderPath);
	}
}
/*
PatchPoints implement a dependency mechanism through their 'links' dictionaries.
Integer indices identify the attenuation value of an inlet
Those same indices as Symbols manage its patching connection
The symbol \value manages the display in NumberBos of the initial value in of the PatchPoint in scaled units
(So a PP scaled by \freq.asSpe will be displayed ranging from 20 - 20000.)

Here is a summary of relevant Views and their interfaces
Slider, Knob - value interface
NumberBox - value but requires special handling when representing the initial value of the PP
Slider2D - x, y
RangeSlider = hi, lo

The classes below fit the interfaces of those different GUI classes to PatchPoint's links scheme and support clean up when
a GUI is deleted.
*/

/* How GUI linking works
PatchPoint.links is a dictionary of dependents indexes by inletNum (for values), inletNum as a Symbol (for patch connections) and \value (for numberboxes displaying scaled values).
update their linked views via a value message, which evaluates 'actionFunc'
That function sends a 'set' message
	The value message evaluates 'actionFunc' which sends a 'set' message to all of its dependants
	This makes it easy to support multi-valued views and might allow one View to control multiple patchpoints,
which could be useful for multi-server uses.
*/

Link {  // connect a PatchPoint to a View using CV's
	classvar onCloseFunc, actionFunc;
	var <>view, <pp,  <index;

	*initClass {
		onCloseFunc = {| view |
			view.dependants.do { |dep | dep.remove };
			view.dependants.release;
		};
		actionFunc = {| view, vals | view.dependants.do { |dep | dep.set(vals) } };
	}

	*new { |view, pp, index | ^super.newCopyArgs(view, pp, index).init }

	*namedNew { |view, moduleName, ppName, index|
		^this.new(view, Module(moduleName).patchPoints[ppName], index)
	}

	initGUIvalue {	| index|	 view.value = pp.stateDict[index]; }

	init {
		view.addDependant(this);
		view.onClose = onCloseFunc;
		view.action = actionFunc;
		pp.links[index] = pp.links[index] .add(this);
		this.initGUIvalue(index);
		^view;
	}

	remove {
		pp !? {
			pp.links[index].remove(this);
			view.onClose = nil;
			view.action = nil;
			view.removeDependant(this);
			pp = nil; index = nil;
		}
	}

	value_ { | val |  view.value =val }				// PatchPoint::update calls value_ to update View
	set { pp.cv_(index, view.value) }
}

LinkV : Link {  // connect from PP to NumberBox, a view presenting scaled values
	*new { |view, pp, index | ^super.newCopyArgs(view, pp, \value).init }

	init {
		view.addDependant(this);
		view.onClose = onCloseFunc;
		view.action = actionFunc;
		pp.links[\value] = pp.links[\value] .add(this);
		view.value = pp.stateDict[\value];
		^view;
	}

	set { pp.value_(view.value) }
}

LinkX : Link {  // connect from PP to view
	*new { |view, pp, index | ^super.newCopyArgs(view, pp, index).init }

	value_ { | val |  view.x =val }
	set { pp.value_(view.x)}
}

LinkY : Link {  // connect from PP to view
	*new { |view, pp, index | ^super.newCopyArgs(view, pp, index).init }

	value_ { | val |  view.y =val }
	set { pp.value_(view.y)}
}

LinkH : Link {  // connect from PP to view
	*new { |view, pp, index | ^super.newCopyArgs(view, pp, index).init }

	value_ { | val |  view.hi =val }
	set { pp.value_(view.hi)}
}

LinkL : Link {  // connect from PP to view
	*new { |view, pp, index | ^super.newCopyArgs(view, pp, index).init }

	value_ { | val |  view.lo =val }
	set { pp.value_(view.lo)}
}

PPLabel : Link {
	//  PPLabel provides a contextual menu for patching
	// This does not apply to inlet 0 of a scaled PatchPoint, which sets an
	// inital value for its associated synth control
	classvar <>defaultSourceMenu;
	classvar <>viewBackPointer, <mouseDownAction, <mouseUpAction;
	var integerIndex;

	*initClass {

		// functions used by Views that link to PatchPoints
		onCloseFunc = {| view |  view.dependants.do { | dep | dep.remove }; view.dependants.release; };
		actionFunc = {| view, value | view.dependants.do { | dep | dep.set(view, value) } };

		mouseDownAction = {| view, x, y, modifiers, button |
			PPLabel.viewBackPointer = view;
			view.dependants.do { | dep | dep.mouseDown(view, x, y, modifiers, button) } };

		mouseUpAction = {| view, x, y, modifiers, button |
			PPLabel.viewBackPointer = view;
			view.dependants.do { | dep | dep.mouseUp(view, x, y, modifiers, button) } };
	}

	*buildSourceMenu { | modules |
		var sources, menu;
		modules = modules ? Module.orderedAll;
		sources = modules.reject { | m | m.outputNames.size == 0 };
		menu = sources.collect({ | m |
			var moduleMenuItems = m.outputNames.collect ({ | outputName |
				MenuAction( outputName, {					// menu provides [moduleName, outletName]
					var srcV = PPLabel.viewBackPointer;
					srcV.action.value( srcV, [m.name,outputName]);
				})
			});
			Menu(*moduleMenuItems).title_(m.name)
		});
		menu = menu.addFirst(
			MenuAction( \nc, {  PPLabel.viewBackPointer.action.value( PPLabel.viewBackPointer, [\nc, ""]) })
		);
		^menu
	}

	*new { |view, pp, index | ^super.newCopyArgs(view, pp, index).init }

	string_ { | string |
		var mod, out;
		if (string.class == String) { string = string.split(Char.space) };
		mod = string[0];
		out = string[1];
		if (mod ==\nc) {
			view.string_((pp.name.asString ++"\n" ++ index));
		} {
			view.string_((mod.asString ++"\n " ++  out));
		};
	}

	string { ^view.string }

	init {
		view.addDependant(this);
		view.onClose = onCloseFunc;
		view.action = actionFunc;

		if (index !=0 || (pp.type != \scaled)) {

			this.sourceMenu_(defaultSourceMenu);
			integerIndex = index;				// use this for patching the inlet
			index = index.asSymbol;			// use this for linking the View
			pp.links[index] = pp.links[index].add(this);
			view.mouseDownAction = mouseDownAction;
			view.mouseUpAction = mouseUpAction;
			this.string_([\nc, ""]);
		} {
			this.string_( [\nc, ""] );  			// scaled PP's use inlet 0 for an initial value and are not patched
			//			view.font.bold_(true);
		}
		^view;
	}

	sourceMenu_ { | menu |
		view.setContextMenuActions(*menu)
	}

	// action receives a pair of a module name and an output name
	set { | v, moduleAndOutput|
		var  mod, out;
		#mod, out = moduleAndOutput;
		 pp.patch( integerIndex, mod, out);
	}

	mouseDown { | view, x, y, modifiers, button |
		var argV;
		if (pp.type == \discrete) {
			if (modifiers bitAnd: 0x80000 != 0) { pp.inletSynths[index.postln].set(\cv, 1) };
			if (modifiers bitAnd: 0x20000 != 0) {
				if (PPGUI.midiCandidate.notNil) {
					argV = PPGUI.midiCandidate[0];
					argV.background = Color(0.86, 0.86, 0.86);
				};
				PPGUI.midiCandidate = [view, pp, integerIndex];		// her's where using symbols as indices creates a complication
				view.background = Color.green;
			} {
				if (PPGUI.midiCandidate.notNil) {
					if (view == PPGUI.midiCandidate[0]) {
						view.background = Color(0.86, 0.86, 0.86);
						PPGUI.midiCandidate = nil;
					}
				};

			}
		}
	}

	mouseUp { | view, x, y, modifiers, button |
		if (pp.type == \discrete) {
			if (modifiers bitAnd: 0x80000 != 0) { pp.inletSynths[index].set(\cv, 0) };
		}
	}

}

PPGUI {
	classvar <>midiCandidate;
	classvar <>maxDecimals = 3;
	classvar <>minDecimals = 0;
	var <pp;

	*new { | patchPoint |
		if (PPLabel.defaultSourceMenu.isNil) { 	// for the moment for convenience
			PPLabel.defaultSourceMenu = PPLabel.buildSourceMenu;
		};
		^super.newCopyArgs(patchPoint)
	}


	numberBox { | pp, index |
		var nb = NumberBox()
							.align_(\center)
							.maxDecimals_(maxDecimals)
							.minDecimals_(maxDecimals);
		if ( index == 0 && { pp.type == \scaled}) {
			^LinkV(nb, pp, index)
		} {
			^Link(nb, pp, index)
		}
	}

	gate { |  index |
		^[PPLabel(StaticText().align_(\center).maxWidth_(100), pp, index)];
	}

	hslider { | index | 	^[
		PPLabel(StaticText().align_(\center).maxWidth_(100), pp, index),
		[Link(Slider().minWidth_(50), pp, index).orientation_(\horizontal), columns: 2],
		this.numberBox(pp, index).maxWidth_(40)
	];
	}

	vslider { | index |
		^[
		[Link(Slider(), pp, index), rows: 2],
		this.numberBox(pp, index),
		PPLabel(StaticText(nil,  Rect(20, 20, 20, 100)).align_(\center), pp, index),
	];
	}

	knob { | index |
		var  argV, k = Knob();
		k.mouseDownAction = { | v, x, y , modifiers |
			if (modifiers bitAnd: 0x20000 != 0) {
				if (PPGUI.midiCandidate.notNil) {
					argV = PPGUI.midiCandidate[0];
					argV.background = Color(0.918, 0.9183, 0.918);
				};
				PPGUI.midiCandidate = [k, pp, index];
				k.background = Color.green
			} {
				if (PPGUI.midiCandidate.notNil) {
					if (v ==PPGUI.midiCandidate[0]) {
						v.background = Color(0.918, 0.9183, 0.918);
						PPGUI.midiCandidate = nil;
					}
				}
			}
		};
		^[VLayout(
			Link(k, pp, index),
			this.numberBox(pp, index),
			PPLabel(StaticText(nil,  Rect(20, 20, 100, 100)).align_(\center), pp, index));
		];
	}

	gates 		{^pp.numInlets.collect { | i | this. gate(i) }	}

	hsliders 	{	^pp.numInlets.collect { | i | this. hslider(i) } }

	vsliders 	{ ^pp.numInlets.collect { | i |  this.vslider(i) } }

	knobs	 	{ ^pp.numInlets.collect { | i |  this.knob(i) } }

}

ModuleGUI1 {
	var <module;
	var <defaultStyle;
	var <gates;
	var <pps;

	*new { | module, defaultStyle = \knobs | ^super.newCopyArgs(module, defaultStyle).init }

	init {
		pps = module.inputNames.asArray.collect { | name | module.patchPoints[name] };
		gates = pps.select { | pp| pp.type == \discrete };
		pps = pps.select { | pp| pp.type != \discrete };
	}

	knobs {
		var  topView, views;
		topView = gates.collect { | gate | PPGUI(gate).gates };
		topView =[
			 StaticText().align_(\center).string_(module.name) .font_(Font("Osaka", 18, true))
		] ++ topView.flatten;
		topView = VLayout(*topView);
		views = pps.collect { | pp | PPGUI(pp).knobs };
		^( views.flatten.addFirst(topView))
	}

}

Patchbay {
	var <>names;
	var <modules;
	var <moduleOutputsMenu;		// hierarchical menu of module outputs
	var <views;									// list of views that can be displayed
	var <>topViews;						// play and preset controls
	var <>window;
	var <>p; 							// preset menu
	var <>midiMapper;

	*new { | names | ^super.newCopyArgs(names).init }
	*gui { | names, parent, rect | ^this.new(names).gui(parent, rect) }

	init {
		if (names.isNil) {
			modules = Module.orderedAll;
			names = Module.orderedAll.collect { | m | m.name };
		} {
			modules = names.flatten.reject(_.isNumber).collect { | name | Module(name) }
		};
		moduleOutputsMenu = PPLabel.buildSourceMenu(modules);
		midiMapper = MIDIMapper();
	}

	gui { | parent, rect |
	var userView, topViews;
		topViews = this.makeTopViews;
		views = names.collect { | n |
			n.asArray.collect { | n |
				if (n.isNumber) {
					{StaticText().string_("  ")}.dup(n)		// creates empty fields in the grid
				} {
					ModuleGUI1(Module(n)).knobs
				}
			}.flatten
		};
		views = views.addFirst(this.makeTopViews);

		window = parent ? Window(bounds: rect? Rect(200,200,200,1000));
		userView = UserView(window, window.bounds.moveTo(0, 0))
			.resize_(5)
			.layout_( GridLayout.rows(*views).spacing_(4));
		window.front;
		SkipJack( { Module.update}, 0.05,name: "module");
	}
	makeTopViews {		// buttons for handling starting and stopping and presets
		var w = Color(0.7, 0.3, 0.3);
		var b = Color.gray(0.9);
	^ [
			Button()
			.states_([ ["play", b, w], ["stop",  w, b]])
			.action_({ | v | if (v.value==1) { Module.play}{ Module.free } }),
			[TextField()
				.align_(\center)
				.string_("add a preset?")
				.mouseDownAction_({|v| v.string_("");})
				.action_({ |v |
					var name = v.string.asSymbol;
					Module.addState( name);							//add current state to list of presets
					p.items = p.items.add(name);					// add the new preset name to the preset selection menu
					v.string_(" preset added!");					// confirm addition of preset
					defer({v.string_("add a preset?")}, 2);		// reset to prompt for new preset after 2 seconds
				}),
				columns: 2
			],
			[p = PopUpMenu()
				.action_({| v | Module.setState(v.item) }),
				columns: 2
			],
			Button()
			.states_([ ["save presets", b, w]])
			.action_({ this.savePresets}),
			Button()
			.states_([ ["load presets", b, w]])
			.action_({ this.loadPresets; }),
			Button()
			.states_([ ["clear presets", b, w]])
			.action_({ Module.states = [];}),
			Button()
			.states_([ ["scan MIDI", b, w]])
			.action_({ midiMapper.init}),
			Button()
			.states_([ ["save MIDI", b, w]])
			.action_({ midiMapper.saveMIDIMappings }),
			Button()
			.states_([ ["load  MIDI", b, w]])
			.action_({ midiMapper.loadMIDIMappings; }),
			Button()
			.states_([ ["clear MIDI", b, w]])
			.action_({ midiMapper.clearMappings }),
		];
	}
	savePresets {
		var presetFolderPath = Platform.userAppSupportDir +/+ "module presets";
		if (pathMatch(presetFolderPath).size == 0) {
			mkdir(presetFolderPath)
		};
		Dialog.savePanel({ arg path;
    		var f = File(path,"wb");
			var addArray;
			addArray = { | array, file |
				file.putString("[\n");
				array.do { |element| file.putString(element.asCompileString ++ ",\n") };
				file.putString("],\n");
			};
			f.putString("[\n");
			Module.states.do { | state |
					// f.putString(state.asCompileString ++ ",\n");
					addArray.(state, f);
			};
			f.putString("]\n");

			f.close;
		}, path: presetFolderPath);
	}

	loadPresets {
		var presetFolderPath = Platform.userAppSupportDir +/+ "module presets";
		 Dialog.openPanel({ arg path;
			Module.states = path.load;
			p.items = Module.states.flop.first;
			p.valueAction_(0);
		}, path: presetFolderPath);
	}
	// moduleDisplayControl { |module, index|
	// 	var w = Color(0.3, 0.3, 0.7);
	// 	var b = Color.gray(0.9);
	// 	^Button()
	// 	.states_([ [module.name, b, w], [module.name, w, b]])
	// 	.action_ ({ | v |
	// 		var mDispList = masterList[index];
	// 		var mViews = mDispList[1..].flat.select({ | v | v.isKindOf(View) });
	// 		var flag = 1 - v.value;
	// 		mViews.do{ |v | v.visible_(flag) };
	// 	});
	// }

}

