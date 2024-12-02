#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),

    Levels = [Line || Line <- string:split(In, "\n", all), Line =/= ""],
    NumLevels = check_levels(Levels),
    io:format("Part 1: ~p~n", [NumLevels]),

    io:format("Part 2: ~p~n", [NumLevels]),

    done;


main(_) ->
    throw({error, "Wrong arguments provided"}).


check_levels(Levels) -> check_levels(Levels, 0).
check_levels([], Agg) -> Agg;
check_levels([RawLevel|Rest], Agg) ->
    Level = get_level(RawLevel),
    case {check_level_asc(Level), check_level_desc(Level)} of 
        {true, _} -> 
            check_levels(Rest, Agg + 1);
        {_, true} -> 
            check_levels(Rest, Agg + 1);
        _ ->
            check_levels(Rest, Agg)
    end.


check_level_asc([H|T]) -> check_level_asc(T, H).
check_level_asc([], _) -> true;
check_level_asc([H|T], Prev) ->
    if (H > Prev), ((H - Prev) =< 3) -> 
        check_level_asc(T, H);
        true -> false
    end.

check_level_desc([H|T]) -> check_level_desc(T, H).
check_level_desc([], _) -> true;
check_level_desc([H|T], Prev) ->
    if (Prev > H), ((Prev - H) =< 3) ->
        check_level_desc(T, H);
        true -> false
    end.
    
get_level(Raw) -> get_level(string:split(Raw, " ", all), []).
get_level([], List) -> List;
get_level([H|T], List) ->
    {N, _} = string:to_integer(H),
    get_level(T, List ++ [N]).

get_body(Cookie) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Url = "https://adventofcode.com/2024/day/2/input",
    Headers = [{"Cookie", "session=" ++ Cookie}],
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, Headers}, HttpOptions, []),
    % io:format("~s", [Body]).
    Body.
