-module(property_tests).
-include_lib("eqc/include/eqc.hrl").

-export([test_all/0,rotors/0]).

-import(enigma,[rotorByName/1, reflectorByName/1, applyRotorInv/4, applyRotor/4]).

alpha_upper() ->
	choose($A,$Z).
	
rotorNames() ->
	elements(["I","II","II","IV","V"]).
	
reflectorNames() ->
	elements(["A","B","C","ThinB","ThinC"]).
	
rotors() ->
	?LET(Name,rotorNames(),
		enigma:rotorByName(Name)).

reflectors() ->
	?LET(Name,reflectorNames(),
		enigma:reflectorByName(Name)).
	
enigma_pairs() ->
	?LET(Reflector,reflectorNames(),
	?LET(Rotors,{rotorNames(),rotorNames(),rotorNames()},
	?LET(Positions,{choose(0,26),choose(0,26),choose(0,26)},
	?LET(PlugBoard,plug_pair(),
	?LET(Offsets,{alpha_upper(),alpha_upper(),alpha_upper()},
	{enigma:setup(Reflector,Rotors,Positions,PlugBoard,Offsets),enigma:setup(Reflector,Rotors,Positions,PlugBoard,Offsets)}))))).

	
%%just generate a very simple plugboard, as more complex ones are harder to
%%generate while avoiding overlapping pairs
plug_pair() ->
	In=choose($A,$Z),
	Out=choose($A,$Z),
	[{In,Out}].
	
%%feeding a letter through a rotor, then back through that rotor in reverse
%%should yield the original letter.
prop_rotor_reverse() ->
	?FORALL(Rotor,rotors(),
		?FORALL(Offset,int(),
			?FORALL(Pos,int(),
				?FORALL(Letter,alpha_upper(),
					enigma:applyRotorInv(Rotor,Offset,Pos,enigma:applyRotor(Rotor,Offset,Pos,Letter)) == Letter
	)))).

%%feeding a letter through a reflector twice
%%should yield the original letter.
prop_reflector_reverse() ->
	?FORALL(Reflector,reflectors(),
		?FORALL(Letter,alpha_upper(),
			enigma:reflect(Reflector,enigma:reflect(Reflector,Letter)) == Letter
	)).

%%feeding a letter through a plugboard twice
%%should yield the original letter.
prop_plugboard_reverse() ->
	?FORALL(PlugBoard,plug_pair(),
		?FORALL(Letter,alpha_upper(),
			enigma:plug(PlugBoard,enigma:plug(PlugBoard,Letter)) == Letter
	)).

%%rotating by an offset, and then zero minus that offset 
%%should yield the original letter.
prop_rotate_reverse() ->
	?FORALL(Offset,int(),
		?FORALL(Letter, alpha_upper(),
			enigma:rotateLetter(enigma:rotateLetter(Letter,Offset),-Offset) == Letter
	)).
	
%%encrypting an decrypting with the same enigma should yield the input text
prop_encrypt_reverse() ->
	?FORALL(Cryptable,list(alpha_upper()),
		?FORALL({EIn,EOut},enigma_pairs(),
			enigma:crypt(EOut,enigma:crypt(EIn,Cryptable)) == Cryptable
		)).

test_all() ->
	eqc:quickcheck(prop_rotor_reverse()),
	eqc:quickcheck(prop_reflector_reverse()),
	eqc:quickcheck(prop_plugboard_reverse()),
	eqc:quickcheck(prop_rotate_reverse()),
	eqc:quickcheck(prop_encrypt_reverse()).