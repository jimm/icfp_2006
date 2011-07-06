-module(test).
-compile(export_all).

set_nth_reg([_R0,R1,R2,R3,R4,R5,R6,R7], 0, Val) ->
    [Val,R1,R2,R3,R4,R5,R6,R7];
set_nth_reg([R0,_R1,R2,R3,R4,R5,R6,R7], 1, Val) ->
    [R0,Val,R2,R3,R4,R5,R6,R7];
set_nth_reg([R0,R1,_R2,R3,R4,R5,R6,R7], 2, Val) ->
    [R0,R1,Val,R3,R4,R5,R6,R7];
set_nth_reg([R0,R1,R2,_R3,R4,R5,R6,R7], 3, Val) ->
    [R0,R1,R2,Val,R4,R5,R6,R7];
set_nth_reg([R0,R1,R2,R3,_R4,R5,R6,R7], 4, Val) ->
    [R0,R1,R2,R3,Val,R5,R6,R7];
set_nth_reg([R0,R1,R2,R3,R4,_R5,R6,R7], 5, Val) ->
    [R0,R1,R2,R3,R4,Val,R6,R7];
set_nth_reg([R0,R1,R2,R3,R4,R5,_R6,R7], 6, Val) ->
    [R0,R1,R2,R3,R4,R5,Val,R7];
set_nth_reg([R0,R1,R2,R3,R4,R5,R6,_R7], 7, Val) ->
    [R0,R1,R2,R3,R4,R5,R6,Val].

test1() ->
    {Time, _Result} = timer:tc(?MODULE, test1_run, []),
    io:format("~p~n", [Time]).

test1_run() ->
    lists:map(fun(_I) -> test1_run_one() end, lists:seq(1, 10000)).

test1_run_one() ->
    Registers = [0, 0, 0, 0, 0, 0, 0, 0],
    set_nth_reg(Registers, 3, 42).

test2() ->
    {Time, _Result} = timer:tc(?MODULE, test2_run, []),
    io:format("~p~n", [Time]).

test2_run() ->
    lists:map(fun(_I) -> test2_run_one() end, lists:seq(1, 10000)).

test2_run_one() ->
    Registers = [0, 0, 0, 0, 0, 0, 0, 0],
    lists:sublist(Registers, 3) ++ [42] ++ lists:sublist(Registers, 3+2, 8 - 1 - 3).

test3() ->
    {Time, _Result} = timer:tc(?MODULE, test3_run, []),
    io:format("~p~n", [Time]).

test3_run() ->
    lists:map(fun(_I) -> test3_run_one() end, lists:seq(1, 10000)).

test3_run_one() ->
    Registers = [0, 0, 0, 0, 0, 0, 0, 0],
    {L1, L2} = lists:split(3, Registers),
    [_OldVal|T] = L2,
    L1 ++ [42] ++ T.

test4() ->
    {Time, _Result} = timer:tc(?MODULE, test4_run, []),
    io:format("~p~n", [Time]).

test4_run() ->
    lists:map(fun(_I) -> test4_run_one() end, lists:seq(1, 10000)).

test4_run_one() ->
    Registers = [0, 0, 0, 0, 0, 0, 0, 0],
    {L1, L2} = lists:split(3, Registers),
    [_OldVal|T] = L2,
    L1 ++ [42|T].

%%% ================================================================

abc_test1() ->
    {Time, _Result} = timer:tc(?MODULE, abc_test1_run, []),
    io:format("~p~n", [Time]).

abc_test1_run() ->
    lists:map(fun(_I) -> test1_run_one() end, lists:seq(1, 10000)).

abc_test1_run_one() ->
    Instruction = 16#f0f0f0f0,
    A = (Instruction bsr 6) band 7,
    B = (Instruction bsr 3) band 7,
    C = Instruction band 7,
    io:format("~p~n", [[A, B, C]]).

abc_test2() ->
    {Time, _Result} = timer:tc(?MODULE, abc_test2_run, []),
    io:format("~p~n", [Time]).

abc_test2_run() ->
    lists:map(fun(_I) -> test2_run_one() end, lists:seq(1, 10000)).

abc_test2_run_one() ->
    Instruction = 16#f0f0f0f0,
    <<_:23, A:3, B:3, C:3>> = <<Instruction:32>>,
    io:format("~p~n", [[A, B, C]]).
