#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),

    Val1 = part1(In),
    io:format("Part 1: ~p~n", [Val1]),

    Val2 = part2(In),
    io:format("Part 2: ~p~n", [Val2]),
    done;


main(_) ->
    throw({error, "Wrong arguments provided"}).


part1(In) ->
    [AllDeps, AllUpdates] = string:split(In, "\n\n", all),
    Deps = [X || X <- string:split(AllDeps, "\n", all), X =/= ""],
    Updates = [X || X <- get_updates(AllUpdates), X =/= ""],
   
    ValidUpdates = get_increasing_updates(Updates, Deps),
    sum_middle_elems(ValidUpdates).

get_increasing_updates(Updates, Deps) -> get_increasing_updates(Updates, Deps, []).
get_increasing_updates([], _, Acc) -> Acc;
get_increasing_updates([L1|Rest], Deps, Acc) ->
    case get_inorder(L1, Deps) of 
        true ->
            get_increasing_updates(Rest, Deps, Acc ++ [L1]);
        _ ->
            get_increasing_updates(Rest, Deps, Acc)
    end.

get_inorder([H|T], Deps) -> get_inorder(T, H, Deps).
get_inorder([], _, _) -> true;
get_inorder([H|T], Prev, Deps) ->
    Dep = integer_to_list(Prev) ++ "|" ++ integer_to_list(H),
    case lists:member(Dep, Deps) of
        true ->
            get_inorder(T, H, Deps);
        _ ->
            false
    end.


sum_middle_elems(L) -> sum_middle_elems(L, 0).
sum_middle_elems([], Acc) -> Acc;
sum_middle_elems([L1|Rest], Acc) ->
    Len = length(L1),
    MidIndex = (Len + 1) div 2,  % 1-indexed language
    Mid = lists:nth(MidIndex, L1),

    sum_middle_elems(Rest, Acc + Mid).
 


part2(In) ->
    [AllDeps, AllUpdates] = string:split(In, "\n\n", all),
    Deps = [X || X <- string:split(AllDeps, "\n", all), X =/= ""],
    Updates = [X || X <- get_updates(AllUpdates), X =/= ""],

    InvalidUpdates = get_invalid_updates(Updates, Deps),

    sum_invalid(InvalidUpdates, Deps).


get_invalid_updates(Updates, Deps) -> get_invalid_updates(Updates, Deps, []).
get_invalid_updates([], _, Acc) -> Acc;
get_invalid_updates([L1|Rest], Deps, Acc) ->
    case get_inorder(L1, Deps) of 
        false ->
            get_invalid_updates(Rest, Deps, Acc ++ [L1]);
        _ ->
            get_invalid_updates(Rest, Deps, Acc)
    end.


sum_invalid(Invalid, Deps) -> sum_invalid(Invalid, Deps, 0).
sum_invalid([], _, Acc) -> Acc;
sum_invalid([L1|L2], Deps, Acc) ->
    RelevantDeps = get_relevant_deps(L1, Deps, []),
    TopoSort = construct_graph(RelevantDeps),

    Len = length(TopoSort),
    MidIndex = (Len + 1) div 2,  % 1-indexed language
    Mid = lists:nth(MidIndex, TopoSort),


    sum_invalid(L2, Deps, Acc + Mid).


get_relevant_deps(_, [], Acc) -> Acc;
get_relevant_deps(L, [Dep|Rest], Acc) ->
    [First, Second] = [list_to_integer(X) || X <- string:split(Dep, "|", all)],
    FirstIn = lists:member(First, L),
    SecondIn = lists:member(Second, L),

    Valid = FirstIn andalso SecondIn,

    case Valid of 
        true ->
            get_relevant_deps(L, Rest, Acc ++ [Dep]);
        _ ->
            get_relevant_deps(L, Rest, Acc)
    end.


get_updates(AllUpdates) ->
    Updates = string:split(AllUpdates, "\n", all),
    get_updates(Updates, []).
    

get_updates([], Acc) -> Acc;
get_updates([H|T], Acc) ->
    Nums = [list_to_integer(X) || X <- string:split(H, ",", all), X =/= ""],
    get_updates(T, Acc ++ [Nums]).

construct_graph(Deps) ->
    {InEdges, OutEdges, Nodes} = construct_graph(Deps, #{}, #{}, sets:new()),
    topo_sort(InEdges, OutEdges, Nodes).
   
 
construct_graph([], InEdges, OutEdges, Nodes) -> {InEdges, OutEdges, Nodes};
construct_graph([H|T], InEdges, OutEdges, Nodes) ->
    [First, Second] = [list_to_integer(X) || X <- string:split(H, "|", all), X =/= ""],
    V1 = maps:get(First, OutEdges, []),
    OutEdges2 = maps:put(First, V1 ++ [Second], OutEdges),

    V2 = maps:get(Second, InEdges, []),
    InEdges2 = maps:put(Second, V2 ++ [First], InEdges),

    Nodes2 = sets:add_element(First, Nodes),
    Nodes3 = sets:add_element(Second, Nodes2),

    construct_graph(T, InEdges2, OutEdges2, Nodes3).

topo_sort(InEdges, OutEdges, Nodes) ->
    EntryNodes = [X || X <- sets:to_list(Nodes), maps:get(X, InEdges, []) =:= []],
    topo_sort(InEdges, OutEdges, Nodes, EntryNodes, [], []).

topo_sort(_, _, _, [], _, Acc) -> Acc;
topo_sort(InEdges, OutEdges, Nodes, Queue, Visited, Acc) ->
    % Remove node from InEdges since we will already add to the Acc
    NewInEdges = maps:map(
        fun(_Key, ValueList) -> 
            [X || X <- ValueList, not lists:member(X, Queue)]
        end,
        InEdges
    ),

    NewVisited = Visited ++ Queue,
    NextQueue = [X || X <- sets:to_list(Nodes), maps:get(X, NewInEdges, []) =:= [], not lists:member(X, NewVisited)],
    case lists:sort(NextQueue) =:= lists:sort(Queue) of
        % Last layer
        true ->
            topo_sort(NewInEdges, OutEdges, Nodes, [], NewVisited, Acc ++ Queue);
        _ ->
            topo_sort(NewInEdges, OutEdges, Nodes, NextQueue, NewVisited, Acc ++ Queue)
    end.
    
 

get_body(Cookie) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Url = "https://adventofcode.com/2024/day/5/input",
    Headers = [{"Cookie", "session=" ++ Cookie}],
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, Headers}, HttpOptions, []),
    Body.
