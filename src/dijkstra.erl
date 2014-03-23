-module(dijkstra).

-export([dijkstra/2,
         dijkstra/3,
         prepare/2,
         dummy/0]).

dijkstra(Graph, Source) ->
    Vertices = digraph:vertices(Graph),
    {Dist, Prev} = prepare(Vertices, Source),
    iterate(Graph, Vertices, Dist, Prev).

dijkstra(_Graph, _Source, _Target) ->
    ok.

iterate(_Graph, [], Dist, _Prev) ->
    Dist;
iterate(Graph, Q, Dist, Prev) ->
    % find the closest next vertex
    case array:foldl(fun(Idx, E, {CurMinIdx, CurMin}) ->
                             Check = lists:member(Idx, Q),
                             case erlang:min(E, CurMin) of
                                 E when Check -> {Idx, E};
                                 _ -> {CurMinIdx, CurMin}
                             end
                     end, {undefined, infinity}, Dist) of
        {undefined, infinity} ->
            % no futher vertices reachable from the source
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
    CurDist = array:get(V, Dist),
    NDist = array:get(U, Dist),
    case NDist of
        infinity ->
            check_neighbours(Graph, RemEdges, U, Dist, Prev);
        _ ->
            case NDist + Weight of
                D when D < CurDist ->
                    NewDist = array:set(V, D, Dist),
                    NewPrev = array:set(V, U, Prev),
                    check_neighbours(Graph, RemEdges, U, NewDist, NewPrev);
                _ ->
                    check_neighbours(Graph, RemEdges, U, Dist, Prev)
            end
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
