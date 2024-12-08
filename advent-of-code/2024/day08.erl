#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),

    Lines = [X || X <- string:split(In, "\n", all), X =/= ""],
    Antinodes = get_antennas(Lines, false),
    io:format("Part 1: ~p~n", [length(sets:to_list(Antinodes))]),

    AntinodesHarmonics = get_antennas(Lines, true),
    io:format("Part 2: ~p~n", [length(sets:to_list(AntinodesHarmonics))]),

    done;

main(_) ->
    throw({error, "Wrong arguments provided"}).


get_antennas(Grid, Harmonics) -> get_antennas(Grid, 1, 1, #{}, sets:new(), Harmonics).
get_antennas(Grid, I, J, Antennas, Antinodes, Harmonics) ->
    case I > length(Grid) of
        true ->
            Antinodes;
        _ ->
            Row = lists:nth(I, Grid),
            case J > length(Row) of
                true ->
                    get_antennas(Grid, I+1, 1, Antennas, Antinodes, Harmonics);
                _ ->
                    Val = lists:nth(J, Row),
                    case Val of
                        $. ->
                            get_antennas(Grid, I, J+1, Antennas, Antinodes, Harmonics);
                        $# ->
                            get_antennas(Grid, I, J+1, Antennas, Antinodes, Harmonics);
                        _ ->
                            Locs = maps:get(Val, Antennas, []),
                            NewLocs = Locs ++ [{I,J}],
                            NewAntennas = maps:put(Val, NewLocs, Antennas),
                            
                            % Use old Locs to not count the antenna with itself
                            NewAntinodes = get_antinodes(Grid, Val, I, J, Locs, Antinodes, Harmonics),
                            get_antennas(Grid, I, J+1, NewAntennas, NewAntinodes, Harmonics)
                    end
            end
    end.


get_antinodes(_, _, _, _, [], Antinodes, _) -> Antinodes;

% Antennas here are for all antennas of a specific frequency
get_antinodes(Grid, Val, I, J, [Antenna|Rest], Antinodes_, Harmonics) ->
    {OtherI, OtherJ} = Antenna,
    DistI = I - OtherI,
    DistJ = J - OtherJ,

    case Harmonics of
        true ->
            Init = sets:add_element({OtherI,OtherJ}, sets:add_element({I,J}, sets:new())),
            Antinodes = sets:union([Antinodes_, Init]);
        _ ->
            Antinodes = Antinodes_
    end,

    case I + DistI > length(Grid) orelse I - DistI < 1 of
        true ->
            AntinodesA = Antinodes;
        _ ->
            RowA = lists:nth(I + DistI, Grid),
            case J + DistJ > length(RowA) orelse J + DistJ < 1 of
                true ->
                    AntinodesA = Antinodes;
                _ ->
                    case Harmonics of
                        true ->
                            HarmonicsA = get_harmonics(Grid, I, J, DistI, DistJ, Antinodes),
                            AntinodesA = sets:add_element({I,J}, HarmonicsA);
    
                        _ ->
                            AntinodesA = sets:add_element({I+DistI, J+DistJ}, Antinodes)
                    end
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
                    case Harmonics of
                        true ->
                            HarmonicsB = get_harmonics(Grid, OtherI, OtherJ, -DistI, -DistJ, AntinodesA),
                            AntinodesB = sets:add_element({OtherI,OtherJ}, HarmonicsB);
                        _ ->
                            AntinodesB = sets:add_element({OtherI-DistI, OtherJ-DistJ}, AntinodesA)
                    end
            end
    end,
    get_antinodes(Grid, Val, I, J, Rest, AntinodesB, Harmonics).


get_harmonics(Grid, I, J, DistI, DistJ, Antinodes) ->
    case I + DistI > length(Grid) orelse I + DistI < 1 of
        true ->
            Antinodes;
        _ ->
            RowA = lists:nth(I + DistI, Grid),
            case J + DistJ > length(RowA) orelse J + DistJ < 1 of
                true ->
                    Antinodes;
                _ ->
                    AntinodesA = sets:add_element({I+DistI, J+DistJ}, Antinodes),
                    get_harmonics(Grid, I+DistI, J+DistJ, DistI, DistJ, AntinodesA)
            end
    end.
            

get_body(Cookie) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Url = "https://adventofcode.com/2024/day/8/input",
    Headers = [{"Cookie", "session=" ++ Cookie}],
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, Headers}, HttpOptions, []),
    Body.
