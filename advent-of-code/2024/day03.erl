#!/usr/bin/env escript
-mode(compile).

main([Cookie]) ->
    In = get_body(Cookie),
    % In = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",
    % In = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))",

    Val1 = find_matches(In, false),
    io:format("Part 1: ~p~n", [Val1]),

    Val2 = find_matches(In, true),
    io:format("Part 2: ~p~n", [Val2]),

    done;

main(_) ->
    throw({error, "Wrong arguments provided"}).


find_matches(Str, Conditionals) ->
    case Conditionals of
        false -> 
            Pattern = "(mul\\((?<num1>\\d{1,4}),(?<num2>\\d{1,4})\\))",
            {ok, MP} = re:compile(Pattern),
            {match, Matches} = re:run(Str, MP, [global, {capture, [num1, num2], list}]);
        _ ->
            Pattern = "(?<do>do\\(\\))|(?<dont>don\\'t\\(\\))|"
                        "(mul\\((?<num1>\\d{1,4}),(?<num2>\\d{1,4})\\))",
            {ok, MP} = re:compile(Pattern),
            {match, RawMatches} = re:run(
                Str, MP, [global, {capture, [do, dont, num1, num2], list}]
            ),
            Matches = filter(RawMatches, true, [])
            
    end,

    aggregate(Matches, 0).


filter([], _, Agg) -> Agg;
filter([L1|Rest], Take, Agg) ->
    [Do, Dont, Num1, Num2] = L1,
    
    case Num1 of
        [] ->
            Nums = ["0", "0"];
        _ ->
            Nums = [Num1, Num2]
    end,

    case Take of
        true ->
            case Dont of
                [] -> 
                    filter(Rest, Take, Agg ++ [Nums]);
                _ ->
                    filter(Rest, false, Agg)
            end;
        false ->
            case Do of
                [] ->
                    filter(Rest, Take, Agg);
                _ ->
                    filter(Rest, true, Agg ++ [Nums])
            end
    end.
     

aggregate([], Agg) -> Agg;
aggregate([L1|Rest], Agg) ->
    Product = mul(L1, 1),
    aggregate(Rest, Product + Agg).


mul([], Agg) -> Agg;
mul([H|T], Agg) -> 
    {N, _} = string:to_integer(H),
    mul(T, Agg * N).


get_body(Cookie) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Url = "https://adventofcode.com/2024/day/3/input",
    Headers = [{"Cookie", "session=" ++ Cookie}],
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, Headers}, HttpOptions, []),
    Body.
