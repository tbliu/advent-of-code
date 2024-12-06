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
    io:format("Grid ~n~s~n", [In]),

    Val1 = traverse(Grid),
    io:format("Part 1 ~p~n", [Val1]),

    done;


main(_) ->
    throw({error, "Wrong arguments provided"}).


traverse(Grid) ->
    {I, J}= findStartPos(Grid),
    io:format("Start pos ~p~n", [{I,J}]),
    traverse(Grid, I, J, -1, 0, sets:new()).


traverse(Grid, I, J, Dx, Dy, Seen) ->
    case I + Dx < 1 orelse I + Dx > length(Grid) of
        true ->
            length(sets:to_list(Seen)) + 1;
        _ ->
            Row = lists:nth(I + Dx, Grid),
            case J + Dy < 1 orelse J + Dy > length(Row) of
                true ->
                    length(sets:to_list(Seen)) + 1;
                _ ->
                    Val = lists:nth(J + Dy, Row),
                    NewSeen = sets:add_element({I,J}, Seen),

                    case Val of
                        $^ ->
                            traverse(Grid, I+Dx, J+Dy, Dx, Dy, NewSeen);
                        $. -> 
                            traverse(Grid, I+Dx, J+Dy, Dx, Dy, NewSeen);
                        $# ->
                            {NewDx, NewDy} = get_new_direction(Dx, Dy),
                            traverse(Grid, I, J, NewDx, NewDy, NewSeen)
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
    % io:format("~s", [Body]).
    Body.
