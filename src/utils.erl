-module(utils).

-compile(export_all).

%% @doc Returns the Nth fibonacci number.
-spec fib(integer()) -> integer().
fib(0) ->
    0;
fib(1) ->
    1;
fib(N) ->
    fib(N-1) + fib(N-2).

%% @doc Recursively create an array containing the first `T` fibonacci numbers.
-spec fib(integer(), integer(), list()) -> list().
fib(I, T, Acc) when I == T ->
    Acc;
fib(I, T, Acc) when I == 0 ->
    fib(I+1, T, [0|Acc]);
fib(I, T, Acc) when I == 1->
    fib(I+1, T, [1|Acc]);
fib(I, T, [H, H2|_Tail]=Acc) ->
    fib(I+1, T, [H+H2|Acc]).
