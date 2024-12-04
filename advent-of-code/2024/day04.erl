#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),
    
    Lines = [Line || Line <- string:split(In, "\n", all), Line =/= ""],
    Val1 = part1(Lines),
    io:format("Part 1: ~p~n", [Val1]),

    Val2 = part2(Lines),
    io:format("Part 2: ~p~n", [Val2]),

    done;

main(_) ->
    throw({error, "Wrong arguments provided"}).


part1(Arr) ->
    lists:sum(
        [part1(Arr, I, J, "X", 0, 0) ||
            I <- lists:seq(1, length(Arr)), 
            J <- lists:seq(1, length(lists:nth(I, Arr)))
        ]
    ).



part1(Arr, I, J, Need, Dx, Dy) ->
    case Need of
        "" -> NextNeed = "X";
        "X" -> NextNeed = "M";
        "M" -> NextNeed = "A";
        "A" -> NextNeed = "S";
        "S" -> NextNeed = ""
    end,

    case {(I < 1) orelse (I > length(Arr)),
          (J < 1) orelse (try J > length(lists:nth(I, Arr)) catch _:_ -> true end)} of
        {true, _} -> 0;
        {_, true} -> 0;
        _ ->
            IthList = lists:nth(I, Arr),
            JthElem = lists:nth(J, IthList),
            if 
                (JthElem =:= hd(Need)) ->
                    if 
                        (Need =:= "S") -> 
                            1;
                        (Dx =:= 0 andalso Dy =:= 0) ->
                            part1(Arr, I+1, J, NextNeed, 1, 0)
                                + part1(Arr, I-1, J, NextNeed, -1, 0)
                                + part1(Arr, I, J+1, NextNeed, 0, 1)
                                + part1(Arr, I, J-1, NextNeed, 0, -1)
                                + part1(Arr, I-1, J-1, NextNeed, -1, -1)
                                + part1(Arr, I+1, J-1, NextNeed, 1, -1)
                                + part1(Arr, I+1, J+1, NextNeed, 1, 1)
                                + part1(Arr, I-1, J+1, NextNeed, -1, 1);
                        (Dx =/= 0 orelse Dy =/= 0) ->
                            part1(Arr, I+Dx, J+Dy, NextNeed, Dx, Dy)
                    end;
                true ->
                    0
            end
    end. 


part2(Arr) ->
    lists:sum(
        [part2(Arr, I, J) ||
            I <- lists:seq(1, length(Arr)), 
            J <- lists:seq(1, length(lists:nth(I, Arr)))
        ]
    ).

part2(Arr, I, J) ->
    PrevI = I - 1,
    NextI = I + 1,

    ValidI = (I < 1) orelse (I > length(Arr)) orelse (PrevI < 1) orelse (NextI > length(Arr)),
    
    PrevJ = J - 1,
    NextJ = J + 1,

    ValidJ = (J < 1)
        orelse (PrevJ < 1)
        orelse (try J > length(lists:nth(I, Arr)) catch _:_ -> true end)
        orelse (try NextJ > length(lists:nth(NextI, Arr)) catch _:_ -> true end),

    case {ValidI, ValidJ} of
        {true, _} -> 0;
        {_, true} -> 0;
        _ ->
            IthList = lists:nth(I, Arr),
            JthElem = lists:nth(J, IthList),
            if
                (JthElem =:= hd("A")) ->
                    TopLeft = lists:nth(PrevJ, lists:nth(PrevI, Arr)),
                    BottomRight = lists:nth(NextJ, lists:nth(NextI, Arr)),

                    TopRight = lists:nth(NextJ, lists:nth(PrevI, Arr)),
                    BottomLeft = lists:nth(PrevJ, lists:nth(NextI, Arr)),

                    if
                        (TopLeft =:= $S andalso BottomRight =:= $M) ->
                            X = true;
                        (TopLeft =:= $M andalso BottomRight =:= $S) ->
                            X = true;
                        true ->
                            X = false
                    end,

                    if 
                        (TopRight =:= $S andalso BottomLeft =:= $M) ->
                            Y = true;
                        (TopRight =:= $M andalso BottomLeft =:= $S) ->
                            Y = true;
                        true ->
                            Y = false
                    end,

                    if 
                        (X andalso Y) ->
                            1;
                        true ->
                            0
                    end;
                    
                true ->
                    0
            end
    end.

    

get_body(Cookie) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Url = "https://adventofcode.com/2024/day/4/input",
    Headers = [{"Cookie", "session=" ++ Cookie}],
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, Headers}, HttpOptions, []),
    Body.
