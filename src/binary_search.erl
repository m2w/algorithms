-module(binary_search).

-export([search/2]).

-type key() :: number() | string() | bitstring().
-type sorted_list() :: list(key()).

%%---------------------------------------------------------------
%% API
%%---------------------------------------------------------------

%% @doc Searches for a value inside a sorted list using binary search.
-spec search(key(), sorted_list()) -> match | nomatch.
search(_Val, []) -> nomatch;
search(Val, [X]) when Val =:= X -> match;
search(_, [_X]) -> nomatch;
search(Val, L) ->
    Mid = length(L) div 2,
    case lists:nth(Mid, L) of
        X when Val =:= X -> match;
        X when Val > X -> search(Val, lists:nthtail(Mid, L));
        _ -> search(Val, lists:sublist(L, Mid))
    end.

%%---------------------------------------------------------------
%% Tests
%%---------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

search_test() ->
    {"binary search finds elements in a sorted list",
     [?assertEqual(binary_search:search(189, lists:seq(1,300)), match),
      ?assertEqual(binary_search:search(731, lists:seq(1, 1032, 9)), nomatch),
      ?assertEqual(binary_search:search(100, lists:seq(32, 53, 7)), nomatch),
      ?assertEqual(binary_search:search(1, []), nomatch),
      ?assertEqual(binary_search:search(1, [1]), match),
      ?assertEqual(binary_search:search(235, lists:seq(123, 999, 4)), match)]}.
