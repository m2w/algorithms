-module(fibonacci_search).
% an implementation of fibonacci search, a binary search variation.

-export([fib_search/2]).

-type sorted_list() :: list().

%%---------------------------------------------------------------
%% API
%%---------------------------------------------------------------

%% @doc Attempts to find an element in a sorted list using fibonacci search.
-spec fib_search(sorted_list(), integer()) -> found | {error, not_found}.
fib_search([], _Elem) ->
    {error, not_found};
fib_search(List, Elem) ->
    Len = length(List),
    Fibs = fib_array(Len),
    fib_search(List, Len, Elem, Fibs).

%%---------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------

%% @private
%% @doc Generates an array where the last element is >= `TargetVal` fibonacci numbers.
-spec fib_array(integer()) -> sorted_list().
fib_array(TargetVal) ->
    lists:reverse(fib(TargetVal, [])).

%% @private
%% @doc Generates an array where the first element is >= `TargetVal` fibonacci numbers.
-spec fib(integer(), list()) -> list().
fib(TargetVal, []) ->
    fib(TargetVal, [0]);
fib(TargetVal, [0]) ->
    fib(TargetVal, [1,0]);
fib(TargetVal, [H|_Tail]=Acc) when H >= TargetVal ->
    Acc;
fib(TargetVal, [H, H2|_Tail]=Acc) ->
    fib(TargetVal, [H+H2|Acc]).

%% @private
%% @doc Given a sorted list, its length, an element to find and a list of fibonacci numbers,
%% attempt to find the target element using fibonacci search.
fib_search(_List, _Len, _Elem, []) ->
    {error, not_found};
fib_search(List, _Len, Elem, [LastElem]) ->
    case lists:nth(LastElem, List) of
	Hit when Hit == Elem ->
	    found;
	_X ->
	    {error, not_found}
    end;
fib_search(List, Len, Elem, [Prev, Next|Fibs]) when Next =< Len ->
    case lists:nth(Next, List) of
	Hit when Hit == Elem ->
	    found;
	X when X > Elem ->     % target element is smaller than current element
	    fib_search(lists:sublist(List, Prev, Next-Prev), Elem);
	_X ->                  % target element is larger than current element
	    fib_search(List, Len, Elem, [Next|Fibs])
    end;
fib_search(List, Len, Elem, [Prev, _Next|_Fibs]) ->
    fib_search(lists:sublist(List, Prev, Len), Elem).

%%---------------------------------------------------------------
%% Tests
%%---------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

fibonacci_search_test() ->
    {"fibonacci search finds elements in a sorted list",
     [?assertEqual(fibonacci_search:fib_search(lists:seq(1,300), 189), found),
      ?assertEqual(fibonacci_search:fib_search(lists:seq(1, 1032, 9), 731), {error, not_found}),
      ?assertEqual(fibonacci_search:fib_search(lists:seq(32, 53, 7), 100), {error, not_found}),
      ?assertEqual(fibonacci_search:fib_search([], 1), {error, not_found}),
      ?assertEqual(fibonacci_search:fib_search([1], 1), found),
      ?assertEqual(fibonacci_search:fib_search(lists:seq(123, 999, 4), 235), found)]}.
