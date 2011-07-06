%%% ICFP 2006 contest VM

-module(vm).
-author("Jim Menard, jimm@io.com").
-export([run/1, run/2]).
-compile(export_all).                           % DEBUG

-define(UINT, 16#ffffffff).
-define(LOADI_CONST_MASK, 16#1ffffff).
-define(N_REGS, 8).

-record(state, {code, arrays, pc, next_pc, registers}).

%-define(DEBUG, true).

-ifdef(DEBUG).
debug(S, Msg) ->
    #state{pc=PC, registers=Regs, code=Code} = S,
    Instruction = array:get(PC, Code),
    Opcode = Instruction bsr 28,
    {A, B, C} = {(Instruction bsr 6) band 7, (Instruction bsr 3) band 7, Instruction band 7},
    case Opcode of
        13 ->
            io:format("~8.16.0b    loadi ~w, ~w", [PC, (Instruction bsr 25) band 7, Instruction band ?LOADI_CONST_MASK]);
        _ ->
            io:format("~8.16.0b ~8s ~p, ~p, ~p", [PC, Msg, A, B, C])
    end,
    io:format("\t; regs = ~p~n", [Regs]).
-else.
debug(_S, _Msg) -> void.
-endif.

% Run the code in the file Path.
run(Path) ->
    S = initialize(Path),
    run_program(S).

% Run the code in the file Path, but stop after MaxInstructions.
run(Path, MaxInstructions) ->
    S = initialize(Path),
    run_program(S, MaxInstructions).

initialize(Path) ->
    Code = read_code(Path),
    Arrays = array:set(0, Code, array:new()),
    #state{code=Code, arrays = Arrays, pc = 0, next_pc = 1, registers = {0, 0, 0, 0, 0, 0, 0, 0}}.

run_program(S) ->
    Instruction = instruction(S),
    Opcode = Instruction bsr 28,
    #state{registers=Regs} = S,
    S1 = execute_instruction(Instruction, Regs, Opcode, S),
    #state{next_pc=NextPC} = S1,
    run_program(S1#state{pc = NextPC, next_pc=NextPC+1}).

run_program(_S, 0) ->
    void;
run_program(S, N) ->
    Instruction = instruction(S),
    Opcode = Instruction bsr 28,
    #state{registers=Regs} = S,
    S1 = execute_instruction(Instruction, Regs, Opcode, S),
    #state{next_pc=NextPC} = S1,
    run_program(S1#state{pc = NextPC, next_pc=NextPC+1}, N-1).

instruction(S) ->
    #state{pc=PC, code=Code} = S,
    array:get(PC, Code).
    
set_array(S, I, J, Val) when is_integer(I), is_integer(J), is_integer(Val) ->
    #state{arrays=Arrays} = S,
    Array = array:get(I, Arrays),
    Array1 = array:set(J, Val, Array),
    S#state{arrays=array:set(I, Array1, Arrays)}.

% Return the I'th array.
array(S, I) when is_integer(I) ->
    #state{arrays=Arrays} = S,
    array:get(I, Arrays).

% Return the J'th element of the I'th array.
array(S, I, J) when is_integer(I), is_integer(J) ->
    Array = array(S, I),
    array:get(J, Array).

% used by execute_instruction(I, 0, S)
if_c_set_a(S, _Regs, _A, _B, 0) ->
    S;
if_c_set_a(S, Regs, A, B, _) ->
    S#state{registers=setelement(A+1, Regs, element(B+1, Regs))}.

% conditional move
% The register A receives the value in register B,
% unless the register C contains 0.
execute_instruction(Instruction, Regs, 0, S) ->
    {A, B, C} = {(Instruction bsr 6) band 7, (Instruction bsr 3) band 7, Instruction band 7},
    debug(S, "move"),
    if_c_set_a(S, Regs, A, B, element(C+1, Regs));

% array index
% The register A receives the value stored at offset
% in register C in the array identified by B.
execute_instruction(Instruction, Regs, 1, S) ->
    {A, B, C} = {(Instruction bsr 6) band 7, (Instruction bsr 3) band 7, Instruction band 7},
    debug(S, "aindex"),
    S#state{registers=setelement(A+1, Regs, array(S, element(B+1, Regs), element(C+1, Regs)))};

% array amendment
% The array identified by A is amended at the offset
% in register B to store the value in register C.
execute_instruction(Instruction, Regs, 2, S) ->
    {A, B, C} = {(Instruction bsr 6) band 7, (Instruction bsr 3) band 7, Instruction band 7},
    debug(S, "amend"),
    Aindex = element(A+1, Regs),
    set_array(S, Aindex, element(B+1, Regs), element(C+1, Regs));

% addition
% The register A receives the value in register B plus 
% the value in register C, modulo 2^32.
execute_instruction(Instruction, Regs, 3, S) ->
    {A, B, C} = {(Instruction bsr 6) band 7, (Instruction bsr 3) band 7, Instruction band 7},
    debug(S, "add"),
    Answer = element(B+1, Regs) + element(C+1, Regs),
    S#state{registers=setelement(A+1, Regs, Answer band ?UINT)};

% multiplication
% The register A receives the value in register B times
% the value in register C, modulo 2^32.
execute_instruction(Instruction, Regs, 4, S) ->
    {A, B, C} = {(Instruction bsr 6) band 7, (Instruction bsr 3) band 7, Instruction band 7},
    debug(S, "mult"),
    Answer = element(B+1, Regs) * element(C+1, Regs),
    S#state{registers=setelement(A+1, Regs, Answer band ?UINT)};

% division
% The register A receives the value in register B
% divided by the value in register C, if any, where
% each quantity is treated treated as an unsigned 32
% bit number.
execute_instruction(Instruction, Regs, 5, S) ->
    {A, B, C} = {(Instruction bsr 6) band 7, (Instruction bsr 3) band 7, Instruction band 7},
    debug(S, "divide"),
    Val = trunc((element(B+1, Regs) band ?UINT) / (element(C+1, Regs) band ?UINT)),
    S#state{registers=setelement(A+1, Regs, Val)};

% not-and
% Each bit in the register A receives the 1 bit if
% either register B or register C has a 0 bit in that
% position.  Otherwise the bit in register A receives
% the 0 bit.
execute_instruction(Instruction, Regs, 6, S) ->
    {A, B, C} = {(Instruction bsr 6) band 7, (Instruction bsr 3) band 7, Instruction band 7},
    debug(S, "not-and"),
    S#state{registers=setelement(A+1, Regs, (bnot (element(B+1, Regs) band element(C+1, Regs))) band ?UINT)};

% halt
% The universal machine stops computation.
execute_instruction(_Instruction, _Regs, 7, _S) ->
    debug(_S, "halt"),
    exit("Halt");

% allocation
% A new array is created with a capacity of platters
% commensurate to the value in the register C. This
% new array is initialized entirely with platters
% holding the value 0. A bit pattern not consisting of
% exclusively the 0 bit, and that identifies no other
% active allocated array, is placed in the B register.
execute_instruction(Instruction, Regs, 8, S) ->
    {B, C} = {(Instruction bsr 3) band 7, Instruction band 7},
    debug(S, "alloc"),
    #state{arrays=Arrays} = S,
    I = array:size(Arrays),
    S1 = S#state{arrays=array:set(I, array:new([{size, element(C+1, Regs)}, {fixed, true}, {default, 0}]), Arrays)},
    S1#state{registers=setelement(B+1, Regs, I)};

% abandonment
% The array identified by the register C is abandoned.
% Future allocations may then reuse that identifier.
% This code does not deallocate the old array.
execute_instruction(Instruction, Regs, 9, S) ->
    C = Instruction band 7,
    debug(S, "abandon"),
    % abandoning the 0 array (the code array) is a failure
    I = element(C+1, Regs),
    case I of
        0 ->
            exit("Can not abandon array 0 (code array)");
        _ ->
            #state{arrays=Arrays} = S,
            S#state{arrays=array:set(I, undefined, Arrays)}
    end;

% output
% The value in the register C is displayed on the console
% immediately. Only values between and including 0 and 255
% are allowed.
execute_instruction(Instruction, Regs, 10, S) ->
    C = Instruction band 7,
    debug(S, "output"),
    Val = element(C+1, Regs),
    io:format("~c", [Val]),
    S;

% input
% The universal machine waits for input on the console.
% When input arrives, the register C is loaded with the
% input, which must be between and including 0 and 255.
% If the end of input has been signaled, then the 
% register C is endowed with a uniform value pattern
% where every place is pregnant with the 1 bit.
execute_instruction(Instruction, Regs, 11, S) ->
    C = Instruction band 7,
    debug(S, "input"),
    Char = io:get_chars("", 1),
    S#state{registers=setelement(C+1, Regs, case Char of
                                                eof -> ?UINT;
                                                _ -> Char
                                            end)};

% load program
% The array identified by the B register is duplicated
% and the duplicate shall replace the '0' array,
% regardless of size. The execution finger is placed
% to indicate the platter of this array that is
% described by the offset given in C, where the value
% 0 denotes the first platter, 1 the second, et
% cetera.
%
% The '0' array shall be the most sublime choice for
% loading, and shall be handled with the utmost
% velocity.
execute_instruction(Instruction, Regs, 12, S) ->
    {B, C} = {(Instruction bsr 3) band 7, Instruction band 7},
    debug(S, "loadprog"),
    Bindex = element(B+1, Regs),
    NewPC = element(C+1, Regs),
    case Bindex of
        0 ->
            S#state{next_pc=NewPC};
        _ ->
            #state{arrays=Arrays} = S,
            Code = array(S, Bindex),
            S#state{code=Code, arrays=array:set(0, Code, Arrays), next_pc=NewPC}
    end;

% orthography
% The value indicated is loaded into the register A
% forthwith.
execute_instruction(Instruction, Regs, 13, S) ->
    debug(S, "loadi"),
    A = (Instruction bsr 25) band 7,
    N = Instruction band ?LOADI_CONST_MASK,
    S#state{registers=setelement(A+1, Regs, N)};

% unknown
execute_instruction(_Instruction, _Regs, _Unknown, _S) ->
    debug(_S, "UNKNOWN"),
    exit("Unknown opcode").

read_code(Path) ->
    {ok, Binary} = file:read_file(Path),
    array:fix(array:from_list(binary_to_longs(Binary, []))).

binary_to_longs(<<>>, CodeList) ->
    lists:reverse(CodeList);
binary_to_longs(Binary, CodeList) ->
    <<Val:32, Rest/binary>> = Binary,
    binary_to_longs(Rest, [Val | CodeList]).
