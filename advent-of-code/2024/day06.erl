#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),
    _ = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...",

    % split string by newline to get each element in list
    Grid = [X || X <- string:split(In, "\n", all), X =/= ""],

    {Unique, Obstructions} = traverse(Grid),
    io:format("Part 1 ~p~n", [Unique]),

    io:format("Part 2 ~p~n", [Obstructions]),

    done;


main(_) ->
    throw({error, "Wrong arguments provided"}).


traverse(Grid) ->
    {I, J}= findStartPos(Grid),
    traverse(Grid, I, J, I, J, -1, 0, sets:new(), sets:new(), sets:new()).


traverse(Grid, I, J, StartI, StartJ, Dx, Dy, Seen, SeenWithDir, Obstructions) ->
    case I + Dx < 1 orelse I + Dx > length(Grid) of
        true ->
            io:format("New visited twice ~p ~n", [sets:to_list(Obstructions)]),
            {length(sets:to_list(Seen)) + 1, length(sets:to_list(Obstructions))};
        _ ->
            Row = lists:nth(I + Dx, Grid),
            case J + Dy < 1 orelse J + Dy > length(Row) of
                true ->
                    io:format("New visited twice ~p ~n", [sets:to_list(Obstructions)]),
                    {length(sets:to_list(Seen)) + 1, length(sets:to_list(Obstructions))};
                _ ->
                    Val = lists:nth(J + Dy, Row),
                   
                    % Used if we hit an obstructiion already there or if we place one 
                    {NewDx, NewDy} = get_new_direction(Dx, Dy),
                    NewSeenWithDir = sets:add_element({I,J,Dx,Dy}, SeenWithDir),
                    NewSeen = sets:add_element({I,J}, Seen),

                    % If we place an obstruction here would we loop?
                    case get_would_loop(Grid, I, J, StartI, StartJ, NewDx, NewDy, SeenWithDir) of
                        true ->
                            case I < 1 orelse I > length(Grid) orelse J < 1 orelse J > length(lists:nth(1, Grid)) of
                                false ->
                                    NewObstructions = sets:add_element({I,J}, Obstructions);
                                _ ->
                                    NewObstructions = Obstructions
                            end;
                        _ ->
                            NewObstructions = Obstructions
                    end;

                    case Val of
                        $# ->
                            traverse(Grid, I, J, StartI, StartJ, NewDx, NewDy, NewSeen, NewSeenWithDir, NewObstructions);

                        _ ->
                            traverse(Grid, I+Dx, J+Dy, StartI, StartJ, Dx, Dy, NewSeen, NewSeenWithDir, NewObstructions)
                    end
            end
    end.


get_would_loop(Grid, I, J, StartI, StartJ, Dx, Dy, Seen) ->
    NewSeen = sets:add_element({I,J,Dx,Dy}, Seen),
    case lists:member({I,J,Dx,Dy}, sets:to_list(Seen)) of
        true ->
            R = lists:nth(I + Dx, Grid),
            V = lists:nth(J + Dy, R),
            case V of
                $. -> true;
                _ -> false
            end;
        _ ->
            case I =:= StartI andalso J =:= StartJ of
                true ->
                    % get_would_loop(Grid, I+Dx, J+Dy, StartI, StartJ, Dx, Dy, NewSeen);
                    false;
                _ ->
                    case I + Dx < 1 orelse I + Dx > length(Grid) of
                        true ->
                            false;
                        _ ->
                            Row = lists:nth(I + Dx, Grid),
                            case J + Dy < 1 orelse J + Dy > length(Row) of
                                true ->
                                    false;
                                _ ->
                                    Val = lists:nth(J + Dy, Row),
                                    case Val of
                                        $# ->
                                            {NewDx, NewDy} = get_new_direction(Dx, Dy),
                                            get_would_loop(Grid, I, J, StartI, StartJ, NewDx, NewDy, NewSeen);  
                                        _ ->
                                            get_would_loop(Grid, I+Dx, J+Dy, StartI, StartJ, Dx, Dy, NewSeen)
                                    end
                            end
                    end
            end
    end.
 
    

get_new_direction(Dx, Dy) ->
    case {Dx, Dy} of
        {-1, 0} ->
            {0, 1};
        {0, 1} ->
            {1, 0};
        {1, 0} ->
            {0, -1};
        {0, -1} ->
            {-1, 0}
    end.        


findStartPos(Grid) -> findStartPos(Grid, 1, 1).
findStartPos(Grid, I, J) ->
    Row = lists:nth(I, Grid),
    case J > length(Grid) of
        true ->
            findStartPos(Grid, I+1, 1);
        _ ->
            Val = lists:nth(J, Row),
            case Val =:= $^ of
                true ->
                    {I,J};
                _ ->
                    findStartPos(Grid, I, J+1)
            end
    end.

    

get_body(Cookie) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Url = "https://adventofcode.com/2024/day/6/input",
    Headers = [{"Cookie", "session=" ++ Cookie}],
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, Headers}, HttpOptions, []),
    Body.
