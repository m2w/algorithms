-module(dijkstra).

-export([dijkstra/2,
         dijkstra/3,
         print_path/3,
         dummy/0]).

dijkstra(Graph, Source) ->
    dijkstra(Graph, Source, full).

dijkstra(Graph, Source, Target) ->
    {Vertices, Dist, Prev} = prepare(Graph, Source),
    iterate(Graph, Vertices, Dist, Prev, Target).

%% TODO: improve the output
print_path(Distances, Hops, Target) ->
    print_path(Distances, Hops, Target, []).
print_path(_Distances, _Hops, start, Route) ->
    Route;
print_path(Distances, Hops, Target, Route) ->
    Prev = array:get(Target, Hops),
    print_path(Distances, Hops, Prev, [Prev|Route]).

iterate(_Graph, [], Dist, Prev, _Target) ->
    {Prev, Dist};
iterate(Graph, UnvisitedVertices, Dist, Prev, Target) ->
    case lists:foldl(fun(Vertex, {ClosestVertex, Distance}) ->
                             D = array:get(Vertex, Dist),
                             case erlang:min(D, Distance) of
                                 D -> {Vertex, D};
                                 _ -> {ClosestVertex, Distance}
                             end
                     end, {undefined, infinity}, UnvisitedVertices) of
        {undefined, infinity} -> %% dead end
            iterate(Graph, [], Dist, Prev, Target);
        {Target, _} -> %% reached target vertex
            iterate(Graph, [], Dist, Prev, Target);
        {NextVertex, _} ->
            NQ = lists:delete(NextVertex, UnvisitedVertices),
            Edges = digraph:out_edges(Graph, NextVertex),
            {NewDist, NewPrev} = check_neighbours(Graph, Edges,
                                                  NQ, Dist, Prev),
            iterate(Graph, NQ, NewDist, NewPrev, Target)
    end.


check_neighbours(_Graph, [], _, Dist, Prev) ->
    {Dist, Prev};
check_neighbours(Graph, [Edge|RemEdges], UnvisitedVertices, Dist, Prev) ->
    {_Name, From, To, Weight} = digraph:edge(Graph, Edge),
    case lists:member(To, UnvisitedVertices) of
        true ->
            CurDist = array:get(To, Dist),
            %% TODO refactor this to make it DRYer
            case array:get(From, Dist) of
                infinity ->
                    check_neighbours(Graph, RemEdges, UnvisitedVertices,
                                     Dist, Prev);
                D when (D+Weight < CurDist) ->
                    NewDist = array:set(To, D+Weight, Dist),
                    NewPrev = array:set(To, From, Prev),
                    check_neighbours(Graph, RemEdges, UnvisitedVertices,
                                     NewDist, NewPrev);
                _ ->
                    check_neighbours(Graph, RemEdges, UnvisitedVertices,
                                     Dist, Prev)
            end;
        false ->
            check_neighbours(Graph, RemEdges, UnvisitedVertices, Dist, Prev)
    end.

prepare(Graph, Source) ->
    Count = digraph:no_vertices(Graph),
    D = array:new([{size, Count}, fixed, {default, infinity}]),
    Dist = array:set(Source, 0, D),
    P = array:new([{size, Count}, fixed, {default, undefined}]),
    Prev = array:set(Source, start, P),
    UnvisitedVertices = digraph:vertices(Graph),
    {UnvisitedVertices, Dist, Prev}.

dummy() ->
    G = digraph:new(),
    digraph:add_vertex(G, 0),
    digraph:add_vertex(G, 1),
    digraph:add_vertex(G, 2),
    digraph:add_vertex(G, 3),
    digraph:add_vertex(G, 4),
    digraph:add_vertex(G, 5),
    digraph:add_vertex(G, 6),
    digraph:add_edge(G, 0, 1, 3),
    digraph:add_edge(G, 0, 3, 5),
    digraph:add_edge(G, 1, 2, 2),
    digraph:add_edge(G, 1, 6, 4),
    digraph:add_edge(G, 2, 4, 6),
    digraph:add_edge(G, 4, 3, 4),
    digraph:add_edge(G, 5, 6, 2),
    digraph:add_edge(G, 5, 2, 8),
    digraph:add_edge(G, 6, 3, 1),
    G.
