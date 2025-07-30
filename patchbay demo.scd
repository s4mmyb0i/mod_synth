(
s.options.blockSize = 2;
s.quit;
Module.clear;
Spec.add(\gate, [0,1,\discrete, 0, 0,0]);
Spec.add(\pw, [0,1,0,0, 0.5]);
Spec.add(\slew, [0,20, 5, 0, 0]);
Spec.add(\q, [0.001,2,'exp',0, 1 ]);
Spec.add(\cutoff, \freq);
Spec.add(\wf, [0.1, 5000, 'exp',0, 100]);
Spec.add(\masterVol,\amp);
Spec.add(\input, \amp);
Spec.add(\in, \amp);


	SynthDef("nc", { | off | Out.kr(off, 0) }).add;

	SynthDef(\ring, {
		var audio = \in1.ar *  \in2.ar;
		Out.ar(\out.kr, LeakDC.ar(audio))
	}).add;

	SynthDef(\noise, { |  nz |
		var audio = WhiteNoise.ar;
		 audio = RLPF.ar(audio, \cutoff.kr(400), \q.kr(0.1,0.1) );
		audio = audio * 1/(Amplitude.kr(audio,0, 0.1) max: 0.1);
		Out.ar(nz, LeakDC.ar(audio))
	}).add;

	SynthDef('S+H', {  | in, rate = 5, pw = 0.5, slew = 0 |
		var clock = LFPulse.kr(rate, pw);
		var sig = Latch.kr( in, clock);
		Out.kr(\out.kr, Lag.kr(sig, slew));
		Out.kr(\clk.kr, clock);
	}).add;

	SynthDef(\sawPulseSinTri, { | saw, pulse, sin, tri |
		var freq = \wf.ar(400) min: 20000;
		var pw = \pw.kr(0.5) min:1 max: 0;
		var scale =freq.linlin(40, 45, 0, 1);						//  interpolation to use LFwaveforms at very slow rates
	// Out.ar(saw, Saw.ar(freq) );
		Out.ar(saw, Saw.ar(freq,scale) - LFSaw.ar(freq, -1, 1-scale) );
	// Out.ar(pulse, LFPulse.ar(freq, 0, pw));
		Out.ar(pulse, Pulse.ar(freq,pw,scale,0.5) +LFPulse.ar(freq, -1, 1 - pw, 1 - scale));
		Out.ar(sin, SinOsc.ar(freq));
		Out.ar(tri, LFTri.ar(freq));
	}).add;


	SynthDef(\adsr, { | gate = 0, a, d, s, r |
		Out.kr(\eg.kr, Env.adsr(a,d, s, r).kr(gate: gate));
	}).add;

	SynthDef(\vcf, { |cutoff, q|
		var audio = RLPF.ar(\input.ar, cutoff, q);
		Out.ar(\lpf.kr, LeakDC.ar(audio));
		Out.ar(\hpf.kr, RHPF.ar(\input.ar, cutoff, q));
	}).add;

	SynthDef (\out, {
		Out.ar(0, LeakDC.ar(\input.ar).dup);
	}).add;

   SynthDef (\kbd, { | kbd, gate |
	Out.kr(kbd, \kbdCtl.kr);
	Out.kr(gate, \gateCtl.kr);
	}).add;

Spec.add(\cfreq, [0,1000, 4]);
Spec.add(\phase, \amp);
Spec.add(\index, [0, 100,2]);
Spec.add(\pow,[0.01, 100, 'exp']);
SynthDef(\fold, { | out, cfreq, index =1, pow |
	Out.ar(out, SinOsc.ar(cfreq, \in.ar * index) **pow )
}).add;

Spec.add(\limit, [-100, 20]);
Spec.add(\fbkLevel, [0,1]);
Spec.add(\nzLevel, [-100, 0]);
Spec.add(\limit, [-100, 0]);
Spec.add(\a, [0.01, 20, 'exp',0, 1]);
Spec.add(\b, \a);
Spec.add(\gain, [0, 20]);
SynthDef(\src, {| out, nzLevel = -40, wcutoff = 16000, gain = 1 , a = 1, b = 10 |
		var sel, exp;
		var audio =LeakDC.ar(\in.ar.tanh * gain) + LPF.ar(PinkNoise.ar(nzLevel.dbamp), wcutoff);
			sel = audio < 0;
		exp = sel * b + (1 - sel * a);
	Out.ar(out,LeakDC.ar(audio  ** exp * gain).tanh);
}).add;

Spec.add(\lag, [0.001, 20, 'exp']);
SynthDef(\gate, { | out, gate, lag |
	Out.ar(out, K2A.ar( LagUD.kr(gate, lag,lag).squared) )
}).add;

//	Module(\nc, \nc);
	Module(\src, \src).setInletNums([a:1, b: 1, wcutoff: 1, gain:1,nzLevel: 1,  in: 3]);
	Module(\fold, \fold).setInletNums([a:1, b: 1, wcutoff: 1, gain:1,nzLevel: 1,  in: 3]);
	Module(\adsr, \adsr).setInletNums([gate: 2, a: 1, d: 1, s: 1, r: 1]);
	Module(\gate1, \gate).setInletNums([gate: 1, lag: 1]);
	Module(\noise, \noise).setInletNums([ cutoff: 2, q: 1]);
 	Module(\vco1, \sawPulseSinTri).setInletNums([wf: 3, pw: 1]);
	Module(\vco2, \sawPulseSinTri).setInletNums([wf: 3, pw: 2]);
	Module(\vco3, \sawPulseSinTri).setInletNums([wf: 3, pw: 1]);
	Module(\ring, \ring).setInletNums([in1: 1,  in2: 1]);
	Module('S+H', 'S+H').setInletNums([in: 1, pw: 1, slew: 1]);
	Module(\vcf, \vcf).setInletNums([input: 4, q: 2, cutoff: 3]);
	Module(\mix, \out);
	Module(\mix).setInletNums([input: 4]);

// Patchbay is a new class that allows you to organize displays along a rectangular gird
// numbers introduce empty spots in the gird for alignment
// The separate class has been introduced to make it easier to define multiple GUI's with the same
// collection of modules.
~pb = Patchbay.gui(
	#[
	[vco1, vco3,  vco2],
	[adsr, gate1, ring],
	[src,  1,  'S+H'],
	[fold,  noise],
	[vcf,   mix]
], rect: Rect(20,200,1200,1000));

s.waitForBoot { };

)

~pb.window.