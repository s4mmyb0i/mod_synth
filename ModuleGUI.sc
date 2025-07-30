/*
To do:
	discrete controls:
		button warp to ID discrete controls....
		trigger management...
			button to trigger but also patch?
		positioned in line with name button...

*/

ModuleGUI {
	classvar <moduleOutputsMenu;		// hierarchical menu of module outputs
	classvar <srcV;									// back pointer used by moduleOutputsMenu action
	classvar <window;
	classvar userView;
	classvar <masterList;						// list of views that can be displayed
	classvar <displayList;						// list of views that are displayed
	classvar p; 											// preset menu

	var <module;
	var <playControl;
	var <inletControls;
	var <displayList;
	var <window;

	*init {
		var moduleMenuItems;						// popup menu of all sources that can be patched
		moduleOutputsMenu =	Module.orderedAll
		.reject { | m | m.outputNames.size == 0 }
		.collect({ | m |
			moduleMenuItems = m.outputNames.collect ({ | outputName |
				MenuAction( outputName, { srcV.action.value(srcV, [m.name,outputName]) })
			});
			Menu(*moduleMenuItems).title_(m.name)
		});
		moduleOutputsMenu = moduleOutputsMenu.addFirst(
				MenuAction( \nc, { srcV.action.value(srcV, [\nc, ""]) })
		);
		masterList = Module.orderedAll
									.collect({ | m, i | ModuleGUI(m, i).displayList })
									.reject { | dl | dl.size ==1 };
		displayList = masterList;
	}

	*gui { | rect, orientation = \rows |

		var w = Color(0.7, 0.3, 0.3);
		var b = Color.gray(0.9);
		var topViews = [
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
					// Module.addState( v.string);			//add to list of presets
					// p.items = Module.states.flop.first;					//
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
			.action_({ ModuleGUI.savePresets}),
		Button()
			.states_([ ["load presets", b, w]])
			.action_({ ModuleGUI.loadPresets; Module.setState()}),
		Button()
			.states_([ ["clear presets", b, w]])
			.action_({ Module.states = [];}),
	];

		SkipJack( { Module.update}, 0.05 ,name: "module");
		window = Window(bounds: rect ? Rect(200,200,200,1000));
		ModuleGUI.init;
		// userView = UserView(window, window.bounds.moveTo(0, 0))
		// .resize_(5)
		// .layout_(GridLayout.rows(*displayList.flatten.addFirst(topViews))
		// .spacing_(1) );
		displayList = displayList.clump((displayList.size/2).roundUp);
		userView = UserView(window, window.bounds.moveTo(0, 0))
			.resize_(5)
			.layout_(
			HLayout(
				GridLayout.rows(*displayList[0].flatten.addFirst(topViews)),
				GridLayout.rows(*(displayList[1].flatten.addFirst([StaticText().maxHeight_(20)])))
			)
			.spacing_(1) );
		^window.front;
	}


	*savePresets {
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

	*loadPresets {
		var presetFolderPath = Platform.userAppSupportDir +/+ "module presets";
		 Dialog.openPanel({ arg path;
			Module.states = path.load;
			p.items = Module.states.flop.first.postln;
		}, path: presetFolderPath);
	}

*new { | module, i = 0 | ^super.new.init(module, i) }

	init { | argModule, index |
		var pps, topLine, gates;
		module = argModule;
		pps = module.inputNames.asArray.collect { | name | module.patchPoints[name] };

		topLine = [this.moduleDisplayControl(module, index) ];
		gates =  pps.select { |pp | pp.type ==\discrete}.collect { | pp |
				pp.numInlets.collect { | index | this.ppCtlHoriz(pp, index)}
		}.flat;
		gates = gates.collect { | g | [g.background_(Color(0.7 ,0.7,1)), columns:2] };
		topLine = topLine ++ gates;

		inletControls = pps.reject { |pp | pp.type ==\discrete}.collect { | pp |
				pp.numInlets.collect { | index |
				this.ppCtlHoriz(pp, index);
			}
		};
		inletControls = inletControls ? [];
		inletControls = inletControls.flatten;

		displayList = inletControls.addFirst(topLine);
	}

	moduleDisplayControl { |module, index|
		var w = Color(0.3, 0.3, 0.7);
		var b = Color.gray(0.9);
		^Button()
		.states_([ [module.name, b, w], [module.name, w, b]])
		.action_ ({ | v |
			var mDispList = masterList[index];
			var mViews = mDispList[1..].flat.select({ | v | v.isKindOf(View) });
			var flag = 1 - v.value;
			mViews.do{ |v | v.visible_(flag) };
		});
	}

	inletPatchControl { | pp, index |
		var t = StaticText().string_("  " ++ pp.name.asString + index).maxHeight_(20);
		t
		.setContextMenuActions(*moduleOutputsMenu)
		.mouseDownAction_( {|v, x, y, modifiers | srcV = t} )
		.action_({|v, modAndOut |
			var mod, out;
			#mod, out = modAndOut;
			if (mod ==\nc) {
				v.string_("  " ++ pp.name + "in"+ index);
			} {
				v.string_("  " ++ mod.asString + out);
			};
			Module.patch( module.name, pp.name, index, mod, out);
		});
		pp.links[index.asSymbol] = pp.links[index.asSymbol].add(t);
		^t;
	}

	gatePatchControl { | pp, index |
		var syn, t = StaticText().string_("  " ++ pp.name.asString + index).maxHeight_(20);
		t
		.setContextMenuActions(*moduleOutputsMenu)
		.mouseDownAction_( {|v, x, y, modifiers | srcV = t;
			if (modifiers bitAnd: 0x20000 != 0) { pp.inletSynths[index].set(\cv, 1) };
		})
		.mouseUpAction_( {|v, x, y, modifiers | srcV = t;
			pp.inletSynths[index].set(\cv, 0)
		} )
		.action_({|v, modAndOut |
			var mod, out;
			#mod, out = modAndOut;
			if (mod ==\nc) {
				v.string_("  " ++ pp.name + "in"+ index);
			} {
				v.string_("  " ++ mod.asString + out);
			};
			Module.patch( module.name, pp.name, index, mod, out);
		});
		pp.links[index.asSymbol] = pp.links[index.asSymbol].add(t);
		^t;
	}

	linkValueView { | pp, view |
		view.action = { | v | pp.value_(view.value) };
		pp.links[\value] = pp.links[\value].add(view)
	}

	linkInputView { | pp, view |
		// view.action = { | v | pp.input_(view.value) };
		// pp.links[\input] = pp.links[\input].add(view)
		view.action = { | v | pp.cv_(0, view.value); pp.changed = pp.changed.add(\value); };
		pp.links[0] = pp.links[0].add(view)
	}

	linkCvView { | pp, index, view |
			view.action = { | v | pp.cv_(index, view.value) };
		pp.links[index] = pp.links[index].add(view) ;
	}

	ppCtl { | pp, index |
		var t, s, n, k;
		case
		{ pp.type == \discrete }
		{ 	t = this.gatePatchControl(pp, index) }

		{index > 0  || (pp.type ==\summed) }
		{
		//if (index > 0  || pp.class ==SummedPatchPoint) {
			t = this.inletPatchControl(pp, index);

			s = Slider().action_({ | v | pp.cv_(index, v.value);  });
			this.linkCvView(pp, index, s);

			n = NumberBox().action_({ | v | pp.cv_(index, v.value) });
			this.linkCvView(pp, index, n);

			// k = Knob().action_({ | v | pp.cv_(index, v.value);  });
			// this.linkCvView(pp, index, k);
		}

		{ index == 0 && (pp.type == \scaled) }
		{
			t = StaticText().string_(pp.name);

			s = Slider().action_({ | v | pp.input_(v.value) });
				this.linkInputView(pp, s);

			n = NumberBox().action_({ | v |  pp.value_(v.value) });
			this.linkValueView(pp, n);

			// k = Knob().action_({ | v | pp.cv_(index, v.value);  });
			// this.linkCvView(pp, index, k);

			};


		^[t, s, n]
	}

	// current use
	ppCtlHoriz { | pp, index|
		var t,s,n;
		#t,s,n = this.ppCtl(pp, index);
		if (s.isNil) { ^[t.font_(Font("Osaka", 14, true)).maxWidth_(80)]};

		^[
			t.font_(Font("Osaka", 14, index == 0)).maxWidth_(80).minHeight_(10),
			[s.minHeight_(10).maxHeight_(30).orientation_(\horizontal), columns: 6],
			n.maxWidth_(60).maxDecimals_(4).minHeight_(10)
		];
	}

	ppCtlVert { | pp, index|
		var t,s,n;
		#t,s,n = this.ppCtl(pp, index);
	^[
			[s	.orientation_(\vertical), rows: 6],
			n.maxWidth_(40).maxHeight_(30).maxDecimals_(4),
			t.font_(Font("Osaka", 14, true)).maxWidth_(80)
		];
	}

	ppSlider { | pp, index |
		var t, s, n;
		if (index== 0) {
			t = StaticText().string_("  " ++ pp.name).font_(Font("Osaka", 14, true)).maxWidth_(40);
			s = Slider()
			.maxHeight_(30)
			.minHeight_(10)
			.action_({ | v | pp.input_(v.value) })
			.orientation_(\horizontal);
			this.linkInputView(pp, s);
			n = NumberBox()
			.action_({ | v |  pp.value_(v.value) })
			.maxWidth_(40).maxDecimals_(4);
			this.linkValueView(pp, n);
		} {
			t = this.inletPatchControl(pp, index);
			s = Slider().orientation_(\horizontal).action_({ | v | pp.cv_(index, v.value);  });
			this.linkCvView(pp, index, s);
			n = NumberBox().action_({ | v | pp.cv_(index, v.value) }).maxWidth_(40).maxHeight_(1000).maxDecimals_(4);
			this.linkCvView(pp, index, n);
		};
		^[t, [s, columns: 6], n]
	}

	ppVSlider { | pp, index |
		var t, s, n;
		if (index== 0) {
			t = StaticText().string_("  " ++ pp.name).font_(Font("Osaka", 14, true));
			s = Slider()
			.action_({ | v | pp.input_(v.value) })
			.orientation_(\horizontal);
			this.linkInputView(pp, s);
			n = NumberBox()
			.action_({ | v |  pp.value_(v.value) })
			.maxWidth_(40).maxHeight_(1000).maxDecimals_(4);
			this.linkValueView(pp, n);
		} {
			t = this.inletPatchControl(pp, index);
			s = Slider().orientation_(\horizontal).action_({ | v | pp.cv_(index, v.value);  });
			this.linkCvView(pp, index, s);
			n = NumberBox().action_({ | v | pp.cv_(index, v.value) }).maxWidth_(40).maxHeight_(1000).maxDecimals_(4);
			this.linkCvView(pp, index, n);
		};
		^[[s, rows:  6], n, t]
	}

	ppKnob { | pp, index |
		var t, k, n;
		if (index== 0) {
			t = StaticText().string_("  " ++ pp.name).font_(Font("Osaka", 14, true));
			k = Knob()
			.action_({ | v | pp.input_(v.value) })
			.orientation_(\horizontal);
			this.linkInputView(pp, k);
			n = NumberBox()
			.action_({ | v |  pp.value_(v.value) })
			.maxWidth_(40).maxHeight_(1000).maxDecimals_(4);
			this.linkValueView(pp, n);
		} {
			t = this.inletPatchControl(pp, index);
			k = Knob().action_({ | v | pp.cv_(index, v.value);  });
			this.linkCvView(pp, index, k);
			n = NumberBox().action_({ | v | pp.cv_(index, v.value) }).maxWidth_(40).maxHeight_(1000).maxDecimals_(4);
			this.linkCvView(pp, index, n);
		};
		^[[k, columns:  2], n, t]
	}



}
/*
PatchPoint links
value:
input:
cv: needs index
patch: [name outputName]

View keeps patchPoint as a property...
View action is defined by PatchPoint
valueAction = { | view | view.getProperty(\patchPoint).value_(v.value) }
inputAction = { | view | view.getProperty(\patchPoint).input_(v.value) }
cvAction = { | view | view.getProperty(\patchPoint).cv_(view.getProperty(\index), v.value)  }
// the following is called by a
patchAction = { | view, srcModule, srcOutput | view.getProperty(\patchPoint).patch(srcModule, srcOutput) }
*/
