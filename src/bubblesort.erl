-module(bubblesort).

% An attempt to emulate basic bubble sort and cocktail sort in Erlang.
% For obvious reasons Erlang isn't suited for an in-place sorting algorithm, but
% it was an interesting exercise.

-export([bubblesort/1,
	 cocktailsort/1]).

-type sorted_list() :: list().
-type unsorted_list() :: list().

%%---------------------------------------------------------------
%% API
%%---------------------------------------------------------------

%% @doc Traverses an unsorted list of integers and returns a sorted list.
%% Uses an approximation of bubble sort.
-spec bubblesort(unsorted_list()) -> sorted_list().
bubblesort(L) ->
    bubblesort(L, []).

%% @doc Traverses an unsorted list of integers and returns a sorted list.
%% Uses an approximation of cocktail sort.
-spec cocktailsort(unsorted_list()) -> sorted_list().
cocktailsort(L) ->
    cocktailsort(L, [], []).

%%---------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------

%% @private
%% @doc Recursively appends the largest element of an unsorted list to an
%% accumulator. Returns the accumulator when the source list is empty.
-spec bubblesort(unsorted_list(), list()) -> sorted_list().
bubblesort([], Res) ->
    Res;
bubblesort(L, Res) ->
    PartiallySorted = bsort(L, []),
    Len = length(PartiallySorted),
    bubblesort(lists:sublist(PartiallySorted, Len-1), [lists:last(PartiallySorted)|Res]).

%% @private
%% @doc Recursively appends the lowest element of an unsorted list to an accumulator
%% and prepends the largest element to another accumulator. Returns both accumulators
%% combined.
-spec cocktailsort(unsorted_list(), sorted_list(), sorted_list()) ->
			  sorted_list().
cocktailsort([], Low, High) ->
    Low ++ High;
cocktailsort(L, Low, High) ->
    LR = bsort(L, []), % left to right
    case length(LR) of
	1 ->
	    cocktailsort([], Low ++ [hd(LR)], High);
	_Longer ->
	    RL = revsort(LR, []),
	    Len = length(RL),
	    cocktailsort(lists:sublist(RL, 2, Len-2), Low ++ [hd(RL)], [lists:last(LR)|High])
    end.

%% @private
%% @doc Iterates through an unsorted list, checking whether the current element is
%% larger than the next, swapping their position in the accumulator if that is the case.
-spec bsort(unsorted_list(), list()) ->
		   list().
bsort([], Acc) ->
    Acc;
bsort([H,TH|T], Acc) when H > TH ->
    bsort([H|T], Acc ++ [TH]);
bsort([H|T], Acc) ->
    bsort(T, Acc ++ [H]).

%% @private
%% @doc Iterates through an unsorted list (right-to-left), checking if the current element
%% is smaller than the previous one, swapping positions in the accumulator if that is the case.
-spec revsort(unsorted_list(), list()) -> list().
revsort([], Acc) ->
    Acc;
revsort(L, Acc) when length(L) == 1 ->
    revsort([], L ++ Acc);
revsort(L, Acc) ->
    Len = length(L),
    Last = lists:last(L),
    PrevToLast = lists:nth(Len-1, L), % care, nth is 1 based
    if Last < PrevToLast ->
	    revsort(lists:sublist(L, Len-2), [Last|[PrevToLast|Acc]]);
	true ->
	    revsort(lists:sublist(L, Len-1), [Last|Acc])
    end.

%%---------------------------------------------------------------
%% Tests
%%---------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

bubblesort_test() ->
    {"bubble sort returns a sorted list",
     [?assertEqual(bubblesort:bubblesort([1,8,3,4]), [1,3,4,8]),
      ?assertEqual(bubblesort:bubblesort([-1,10,-50,23,0]), [-50,-1,0,10,23]),
      ?assertEqual(bubblesort:bubblesort([]), []),
      ?assertEqual(bubblesort:bubblesort([3,3,3,3]), [3,3,3,3])]}.

cocktailsort_test() ->
    {"cocktail sort returns a sorted list",
     [?assertEqual(bubblesort:cocktailsort([10,8,-3,4]), [-3,4,8,10]),
      ?assertEqual(bubblesort:cocktailsort([-1,10,-50,23,0]), [-50,-1,0,10,23]),
      ?assertEqual(bubblesort:cocktailsort([]), []),
      ?assertEqual(bubblesort:cocktailsort([1,2,3,4]), [1,2,3,4]),
      ?assertEqual(bubblesort:cocktailsort([3,3,3,3]), [3,3,3,3])]}.
