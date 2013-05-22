-module(dijkstra).

-export([dijkstra/2,
	 dijkstra/3,
	 prepare/2,
	 dummy/0]).

-define(MAX, 1000). % initial value for finding the min in an array

dijkstra(Graph, Source) ->
    Vertices = digraph:vertices(Graph),
    {Dist, Prev} = prepare(Vertices, Source),
    iterate(Graph, Vertices, Dist, Prev).


dijkstra(Graph, Source, Target) ->
    ok.


iterate(_Graph, [], Dist, _Prev) ->
    Dist;
iterate(Graph, Q, Dist, Prev) ->
    case array:foldl(fun(Idx, E, {CurMinIdx, CurMin}) ->
			     case erlang:min(E, CurMin) of
				 E -> {Idx, E};
				 CurMin -> {CurMinIdx, CurMin}
			     end
		     end, {undefined, ?MAX}, Dist) of
	infinity ->
	    iterate(Graph, [], Dist, Prev);
	{U, _} ->
	    NQ = lists:delete(U, Q),
	    Edges = digraph:out_edges(Graph, U),
	    {NewDist, NewPrev} = check_neighbours(Graph, Edges, U, Dist, Prev),
	    iterate(Graph, NQ, NewDist, NewPrev)
    end.


check_neighbours(_Graph, [], _, Dist, Prev) ->
    {Dist, Prev};
check_neighbours(Graph, [Edge|RemEdges], U, Dist, Prev) ->
    {_Name, _From, V, Weight} = digraph:edge(Graph, Edge),
    Cur = array:get(V, Dist),
    case array:get(U, Dist) + Weight of
       D when D < Cur ->
	    NDist = array:set(V, D, Dist),
	    NPrev = array:set(V, U, Prev),
	    check_neighbours(Graph, RemEdges, U, NDist, NPrev);
	_ ->
	    check_neighbours(Graph, RemEdges, U, Dist, Prev)
    end.



prepare(Vertices, Source) ->
    Count = length(Vertices),
    D = array:new(Count, {default, infinity}),
    Dist = array:set(Source, 0, D),
    Prev = array:new(Count, {default, undefined}),
    {Dist, Prev}.


dummy() ->
    G = digraph:new(),
    digraph:add_vertex(G, 0),
    digraph:add_vertex(G, 1),
    digraph:add_vertex(G, 2),
    digraph:add_vertex(G, 3),
    digraph:add_edge(G, 0, 0, 1, 3),
    digraph:add_edge(G, 1, 0, 2, 5),
    digraph:add_edge(G, 2, 1, 3, 15),
    digraph:add_edge(G, 3, 1, 2, 1),
    digraph:add_edge(G, 4, 3, 1, 6),
    G.
