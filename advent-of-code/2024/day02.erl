#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),

    Levels = [Line || Line <- string:split(In, "\n", all), Line =/= ""],
    NumLevels = check_levels(Levels, false),
    io:format("Part 1: ~p~n", [NumLevels]),

    NumLevelsLeeway = check_levels(Levels, true),
    io:format("Part 2: ~p~n", [NumLevelsLeeway]),

    done;

main(_) ->
    throw({error, "Wrong arguments provided"}).

check_levels(Levels, HasLeeway) -> check_levels(Levels, HasLeeway, 0).
check_levels([], _, Agg) -> Agg;
check_levels([RawLevel|Rest], HasLeeway, Agg) ->
    Level = get_level(RawLevel),
    AscOk = check_level_asc(Level, HasLeeway),
    DescOk = check_level_desc(Level, HasLeeway),

    case {AscOk, DescOk} of
        {true, _} ->
            check_levels(Rest, HasLeeway, Agg + 1);
        {_, true} ->
            check_levels(Rest, HasLeeway, Agg + 1);
        _ ->
            check_levels(Rest, HasLeeway, Agg)
    end.

check_level_asc([H|T], HasLeeway) -> check_level_asc(T, H, undefined, HasLeeway).
check_level_asc([], _, _, _) -> true;
check_level_asc([H|T], Prev, PrevPrev, HasLeeway) ->
    if
        (H > Prev) andalso ((H - Prev) =< 3) ->
            check_level_asc(T, H, Prev, HasLeeway);
        HasLeeway ->
            Option1 = case PrevPrev of
                undefined ->
					% Special case at start: Remove the head of the list (Prev)
                    check_level_asc(T, H, undefined, false);
                _ ->
                    if
                        (H > PrevPrev) andalso ((H - PrevPrev) =< 3) ->
							% Remove the previous element (Prev)
                            check_level_asc(T, H, PrevPrev, false);
                        true ->
                            false
                    end
            end,

			% Remove the current element (H)
            Option2 = check_level_asc(T, Prev, PrevPrev, false),
            Option1 orelse Option2;
        true ->
            false
    end.

check_level_desc([H|T], HasLeeway) -> check_level_desc(T, H, undefined, HasLeeway).
check_level_desc([], _, _, _) -> true;
check_level_desc([H|T], Prev, PrevPrev, HasLeeway) ->
    if
        (Prev > H) andalso ((Prev - H) =< 3) ->
            check_level_desc(T, H, Prev, HasLeeway);
        HasLeeway ->
            Option1 = case PrevPrev of
                undefined ->
                    check_level_desc(T, H, undefined, false);
                _ ->
                    if
                        (PrevPrev > H) andalso ((PrevPrev - H) =< 3) ->
                            check_level_desc(T, H, PrevPrev, false);
                        true ->
                            false
                    end
            end,
            Option2 = check_level_desc(T, Prev, PrevPrev, false),
            Option1 orelse Option2;
        true ->
            false
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
    Body.
