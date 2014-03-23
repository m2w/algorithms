%% An implementation of LSD radix sort.
-module(radix_sort).

-export([lsd/2]).

-type unsorted_list(X) :: list(X).
-type sorted_list(X) :: list(X).

%%---------------------------------------------------------------
%% API
%%---------------------------------------------------------------

%% @doc Sorts a list of integers in an arbitrary radix using radix sort.
-spec lsd(unsorted_list(integer()), Radix :: integer()) -> sorted_list(integer()).
lsd([], _) ->
    [];
lsd(List, Radix) ->
    Steps = trunc(log(lists:max(List), Radix)) + 1,
    lsd(List, Radix, 0, Steps).

%%---------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------

%% @private
lsd(List, _Radix, Digit, Lim) when Digit =:= Lim ->
    List;
lsd(List, Radix, Digit, Lim) ->
    A = array:from_orddict(buckets(List, Radix, Digit, orddict:new()), []),
    NList = array:foldl(fun(_, Val, Acc) -> Acc ++ Val end, [], A),
    lsd(NList, Radix, Digit + 1, Lim).

%% @private
%% @doc Folds the values of the input list into an orddict with the digits as key.
buckets([], _, _, O) ->
    O;
buckets([H|T], R, D, O) ->
    buckets(T, R, D, orddict:append(digit(H, R, D), H, O)).

%% @private
log(X, Base) ->
    math:log(X) / math:log(Base).

%% @private
%% @doc Returns the value of digit for X.
digit(X, Radix, Digit) ->
    X div trunc(math:pow(Radix, Digit)) rem Radix.


%%---------------------------------------------------------------
%% Tests
%%---------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

fibonacci_search_test() ->
    {"radix sort sorts a list of unsorted integers",
     [?assertEqual(radix_sort:lsd([139,2,123,8234,72392,128,1], 10),
                   [1,2,123,128,139,8234,72392]),
     ?assertEqual(radix_sort:lsd([2, 100, 31, 2, 109], 2), [2, 2, 31, 100, 109]),
     ?assertEqual(radix_sort:lsd([], 10), []),
     ?assertEqual(radix_sort:lsd([10], 10), [10])]}.
