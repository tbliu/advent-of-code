#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),

    Lines = [X || X <- string:split(In, "\n", all), X =/= ""],
    Val1 = sum_valid_lines(Lines, false),
    io:format("Part 1: ~p~n", [Val1]),

    Val2 = sum_valid_lines(Lines, true),
    io:format("Part 2: ~p~n", [Val2]),

    done;

main(_) ->
    throw({error, "Wrong arguments provided"}).

sum_valid_lines(Lines, Concat) -> sum_valid_lines(Lines, Concat, 0).
sum_valid_lines([], _, Agg) -> Agg;
sum_valid_lines([L1|Rest], Concat, Agg) ->
    [TargetStr, NumsStr] = string:split(L1, ": ", all),
    {Target, _} = string:to_integer(TargetStr),
    Nums = [N || {N, _} <- [string:to_integer(N) || N <- string:split(NumsStr, " ", all)]],

    case is_valid(Target, Concat, Nums) of
        true ->
            sum_valid_lines(Rest, Concat, Agg + Target);
        _ ->
            sum_valid_lines(Rest, Concat, Agg)
    end.

is_valid(Target, Concat, [H|T]) -> is_valid(Target, Concat, T, H).
is_valid(Target, _, [], Agg) -> Target =:= Agg;
is_valid(Target, Concat, [H|T], Agg) ->
    case Agg > Target of
        true ->
            false;
        _ ->
            case Concat of
                true -> 
                    A = integer_to_list(Agg),
                    B = integer_to_list(H),
                    {ConcatAgg, _} = string:to_integer(A ++ B),
                    is_valid(Target, Concat, T, Agg * H) orelse
                    is_valid(Target, Concat, T, Agg + H) orelse
                    is_valid(Target, Concat, T, ConcatAgg);
                _ ->
                    is_valid(Target, Concat, T, Agg * H) orelse
                    is_valid(Target, Concat, T, Agg + H)
            end
    end.


get_body(Cookie) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Url = "https://adventofcode.com/2024/day/7/input",
    Headers = [{"Cookie", "session=" ++ Cookie}],
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, Headers}, HttpOptions, []),
    Body.
