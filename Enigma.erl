-module(enigma).

-export([rotateLetter/2,rotorByName/1,reflectorByName/1,reflect/2,plug/2,applyRotor/4,applyRotorInv/4]).
-export([reflector/1,rotor/5,plugboard/2,keyboard/2,enigma/2,crypt_one/2,init_enigma/5]).
-export([setup/5,crypt/2,kill/1]).

-include("Enigma.hrl").

reflectorByName(Name)->
	case Name of
		"A" -> reflectorA();
		"B" -> reflectorB();
		"C" -> reflectorC();
		"ThinB" -> reflectorThinB();
		"ThinC" -> reflectorThinC();
		_Else -> erlang:error("Invalid Reflector Name: "++Name)
	end.
	
rotorByName(Name) ->
	case Name of
		"I" -> rotorI();
		"II" -> rotorII();
		"III" -> rotorIII();
		"IV" -> rotorIV();
		"V" -> rotorV();
		_Else -> erlang:error("Invalid Rotor Name: "++Name)
	end.
	
%%shared 2way map lookup for reflect and plug
%%for a mapping and an input a tries to find
%%a pair {a,B}, or {B,a}, and outputs {ok,B} or false
%% if no such entry is found
twoway_map_lookup(Mapping,From) ->
	case lists:keyfind(From,1,Mapping) of
		false ->
			case lists:keyfind(From,2,Mapping) of
				false ->
					false;
				{To,_} ->
					{ok,To}
			end;
		{_,To} ->
			{ok,To}
	end.
	
reflect(Mapping,From) ->
	case twoway_map_lookup(Mapping,From) of
		{ok,To} -> To;
		%in reflection a missing case is a bug
		false -> erlang:error("Reflector is invalid, can't reflect:"++From)
	end.
	
plug(Mapping,From) ->
	case twoway_map_lookup(Mapping,From) of
		{ok,To} -> To;
		%in the plugboard a missing case means we pass the value unchanged
		false -> From
	end.

reflector(Reflector) ->
	receive
		{input,Pid,LetterIn} ->
			Out = reflect(Reflector,LetterIn),
			Pid ! {output, Out},
			reflector(Reflector)
	end.

%rotate letter is used to simulate rotating the wheels
%as instead of rotating the list of all pairs of letters
%"forward" we can instead rotate the input letters "backwards"
rotateLetter(Letter,N) when N < 0 ->
	rotateLetter(Letter,26+N);

rotateLetter(Letter,N) ->
	(((Letter + N) - $A) rem 26) + $A.
	
applyRotor(Rotor,RingSetting,Position,Letter) ->
	RotatedLetter =  rotateLetter(Letter, -(Position + RingSetting)),
	case lists:keyfind(RotatedLetter,1,Rotor) of
		false -> erlang:error("This should be unreachable");
		{_,To} -> To
	end.
	
applyRotorInv(Rotor,RingSetting,Position,Letter) ->
	ScrambledLetter = case lists:keyfind(Letter,2,Rotor) of
		false -> erlang:error("This should be unreachable");
		{To,_} -> To
	end,
	rotateLetter(ScrambledLetter, (Position + RingSetting)).
	

	
rotor(Rotor,RingSetting,Position,InwardsPid,IsLastRotor) ->
	receive
		{inc,Pid} ->
			NewPosition = case Position of
				26 -> 0;
				_ -> Position + 1
			end,
			case IsLastRotor or (NewPosition /= 0) of
				true ->
					Pid ! {incdone};
				false ->
					InwardsPid ! {inc,self()},
					receive 
						{incdone} ->
							Pid ! {incdone}
					end
			end,
			rotor(Rotor,RingSetting,NewPosition,InwardsPid,IsLastRotor);
		{input,Pid,LetterIn} ->
			RotatedIn = applyRotor(Rotor,RingSetting,Position,LetterIn),
			InwardsPid ! {input, self(), RotatedIn},
			receive
				{output,LetterOut} ->
					RotatedOut = applyRotorInv(Rotor,RingSetting,Position,LetterOut),
					Pid ! {output,RotatedOut}
			end,
			rotor(Rotor,RingSetting,Position,InwardsPid,IsLastRotor)
	end.
	
plugboard(PlugboardPairs,InwardsPid) ->
	receive
		{input,Pid,LetterIn} ->
			InwardsPid ! {input,self(),plug(PlugboardPairs,LetterIn)},
			receive
				{output,LetterOut} ->
					Pid ! {output,plug(PlugboardPairs,LetterOut)}
			end
	end,
	plugboard(PlugboardPairs,InwardsPid).
	
keyboard(InwardsPid,RotorPid) ->
	RotorPid ! {inc,self()},
	receive
		{input,Pid,LetterIn} ->
			InwardsPid ! {input,self(),LetterIn},
			receive
				{output,LetterOut} ->
					Pid !  {output,LetterOut}
			end
	end,
	receive
		{incdone} ->
			%wait for the increment to finish so that we don't potentially desynchronise
			ok
	end,
	keyboard(InwardsPid,RotorPid).
	
enigma(KbdPid,ChildPids) ->
	receive
		{input,Pid,LetterIn} ->
			KbdPid ! {input,self(),LetterIn},
			receive
				{output,LetterOut} ->
					Pid ! {output,LetterOut}
			end,
			enigma(KbdPid,ChildPids)
	end.
	
encryptables()->
	lists:seq($A,$Z).
	
crypt_one(Enigma,Letter) ->
	LetterUpper = string:to_upper(Letter),
	case lists:member(LetterUpper,encryptables()) of
		true ->
			Enigma ! {input,self(),LetterUpper},
			receive
				{output,LetterOut} ->
					LetterOut
			end;
		false -> Letter
	end.

crypt(Enigma,String) ->
	lists:map(fun (Letter)-> crypt_one(Enigma,Letter) end,String).
	
init_enigma(ReflectorName,RotorNames,RingSettings,PlugboardPairs,InitialSettings)->
	ReflectorPid = spawn_link(?MODULE,reflector,[reflectorByName(ReflectorName)]),
	Rotor1Pid = spawn_link(?MODULE,rotor,[rotorByName(element(1,RotorNames)),element(1,RingSettings),element(1,InitialSettings)-$A,ReflectorPid,true]),
	Rotor2Pid = spawn_link(?MODULE,rotor,[rotorByName(element(2,RotorNames)),element(2,RingSettings),element(2,InitialSettings)-$A,Rotor1Pid,false]),
	Rotor3Pid = spawn_link(?MODULE,rotor,[rotorByName(element(3,RotorNames)),element(3,RingSettings),element(3,InitialSettings)-$A,Rotor2Pid,false]),
	PlugBoardPid = spawn_link(?MODULE,plugboard,[PlugboardPairs,Rotor3Pid]),
	KbdPid = spawn_link(?MODULE,keyboard,[PlugBoardPid,Rotor3Pid]),
	enigma(KbdPid,[ReflectorPid,Rotor1Pid,Rotor2Pid,Rotor3Pid,PlugBoardPid,KbdPid]).

setup(ReflectorName,RotorNames,RingSettings,PlugboardPairs,InitialSettings)->
	spawn(?MODULE,init_enigma,[ReflectorName,RotorNames,RingSettings,PlugboardPairs,InitialSettings]).
	
%%Kill an enigma process
kill(Enigma) ->
	%Just need to kill the root as the other processes are linked to it
	exit(Enigma,kill).
	
%TODO: verifiy using known enigma messages, may not be possible as turnover positions are not modeled