#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),
    _ = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............",
    _ = "..........
...#......
#.........
....a.....
........a.
.....a....
..#.......
......A...
..........
..........",

    Lines = [X || X <- string:split(In, "\n", all), X =/= ""],
    {Antennas, Antinodes} = get_antennas(Lines),
    io:format("vals ~w~n", [sets:to_list(Antinodes)]),
    % io:format("Part 1: ~p~n", [count_map_values(Antinodes)]),
    io:format("Part 1: ~p~n", [length(sets:to_list(Antinodes))]),

    done;

main(_) ->
    throw({error, "Wrong arguments provided"}).


get_antennas(Grid) -> get_antennas(Grid, 1, 1, #{}, sets:new()).
get_antennas(Grid, I, J, Antennas, Antinodes) ->
    case I > length(Grid) of
        true ->
            {Antennas, Antinodes};
        _ ->
            Row = lists:nth(I, Grid),
            case J > length(Row) of
                true ->
                    get_antennas(Grid, I+1, 1, Antennas, Antinodes);
                _ ->
                    Val = lists:nth(J, Row),
                    case Val of
                        $. ->
                            get_antennas(Grid, I, J+1, Antennas, Antinodes);
                        $# ->
                            get_antennas(Grid, I, J+1, Antennas, Antinodes);
                        _ ->
                            Locs = maps:get(Val, Antennas, []),
                            NewLocs = Locs ++ [{I,J}],
                            NewAntennas = maps:put(Val, NewLocs, Antennas),
                            
                            % Use old Locs to not count the antenna with itself
                            NewAntinodes = get_antinodes(Grid, Val, I, J, Locs, Antinodes),
                            get_antennas(Grid, I, J+1, NewAntennas, NewAntinodes)
                    end
            end
    end.


get_antinodes(_, _, _, _, [], Antinodes) -> Antinodes;

% Antennas here are for all antennas of a specific frequency
get_antinodes(Grid, Val, I, J, [Antenna|Rest], Antinodes) ->
    {OtherI, OtherJ} = Antenna,
    DistI = I - OtherI,
    DistJ = J - OtherJ,

    case I + DistI > length(Grid) orelse I - DistI < 1 of
        true ->
            AntinodesA = Antinodes;
        _ ->
            RowA = lists:nth(I + DistI, Grid),
            case J + DistJ > length(RowA) orelse J + DistJ < 1 of
                true ->
                    AntinodesA = Antinodes;
                _ ->
                    io:format("Adding A ~p ~n", [{I+DistI, J+DistJ}]),
                    AntinodesA = sets:add_element({I+DistI, J+DistJ}, Antinodes)
            end
    end,
            
    case OtherI - DistI < 1 orelse OtherI + DistI > length(Grid) of
        true ->
            AntinodesB = AntinodesA;
        _ ->
            RowB = lists:nth(OtherI - DistI, Grid),
            case OtherJ - DistJ < 1 orelse OtherJ - DistJ > length(RowB) of
                true ->
                    AntinodesB = AntinodesA;
                _ ->
                    io:format("Adding B Origin ~p val ~p ~n", [{I,J}, {OtherI-DistI, OtherJ-DistJ}]),
                    AntinodesB = sets:add_element({OtherI-DistI, OtherJ-DistJ}, AntinodesA)
            end
    end,
    get_antinodes(Grid, Val, I, J, Rest, AntinodesB).


get_body(Cookie) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Url = "https://adventofcode.com/2024/day/8/input",
    Headers = [{"Cookie", "session=" ++ Cookie}],
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, Headers}, HttpOptions, []),
    Body.
