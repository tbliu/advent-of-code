#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),

    % split string by newline to get each element in list
    Lines = string:split(In, "\n", all),
    {First, Second} = split_list(Lines),

    Sorted1 = lists:sort(First),
    Sorted2 = lists:sort(Second),

    Diffs = sum_diffs(Sorted1, Sorted2),
    io:format("Part 1: ~p~n", [Diffs]),

    Counts = get_count(Sorted2),
    Sim = sum_similarity(Sorted1, Counts),
    io:format("Part 2: ~p~n", [Sim]),

    done;


main(_) ->
    throw({error, "Wrong arguments provided"}).

split_list(List) -> split_list(List, [], []).
split_list([], First, Second) -> {First, Second};
split_list([H|T], First, Second) ->
    case string:split(H, "   ", all) of
        [F, S] -> split_list(T, First ++ [F], Second ++ [S]);
        _ -> split_list(T, First, Second)
    end.


sum_diffs(L1, L2) -> sum_diffs(L1, L2, 0).
sum_diffs([], [], Agg) -> Agg;
sum_diffs([H1|T1], [H2|T2], Agg) ->
    {N1, _} = string:to_integer(H1),
    {N2, _} = string:to_integer(H2),
    Diff = abs(N1 - N2),
    sum_diffs(T1, T2, Agg + Diff).


get_count(List) -> get_count(List, #{}).
get_count([], Map) -> Map;
get_count([H|T], Map) ->
    {N, _} = string:to_integer(H),
    case maps:find(N, Map) of
        {ok, Value} ->
            M = maps:put(N, Value + 1, Map);
        error ->
            M = maps:put(N, 1, Map)
    end,
    get_count(T, M).


sum_similarity(L, M) -> sum_similarity(L, M, 0).
sum_similarity([], M, Agg) -> Agg;
sum_similarity([H|T], M, Agg) ->
    {N, _} = string:to_integer(H),
    Value = maps:get(N, M, 0),
    V = Value * N,
    sum_similarity(T, M, Agg + V).
    


get_body(Cookie) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Url = "https://adventofcode.com/2024/day/1/input",
    Headers = [{"Cookie", "session=" ++ Cookie}],
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, Headers}, HttpOptions, []),
    % io:format("~s", [Body]).
    Body.
