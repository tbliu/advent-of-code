using HTTP
using JSON3

function _get_next(str_sequence::SubString{String})::Int
    sequence = [tryparse(Int64, x) for x in split(strip(str_sequence), " ")]
    steps = []
    curr = sequence

    while !all(x -> x === 0, curr)
        push!(steps, curr)
        curr = diff(curr)
    end

    adder = 0
    for seq in reverse(steps)
        last = seq[end]
        adder += last
    end
    return adder
end

function _get_first(str_sequence::SubString{String})::Int
    sequence = [tryparse(Int64, x) for x in split(strip(str_sequence), " ")]
    steps = []
    curr = sequence

    while !all(x -> x === 0, curr)
        push!(steps, curr)
        curr = diff(curr)
    end

    prefix = 0
    for seq in reverse(steps)
        first = seq[1]
        prefix = first - prefix
    end

    return prefix
end

function part1(sequences::Vector{SubString{String}})::Int
    total = 0
    for seq in sequences
        total += _get_next(seq)
    end

    println("Part 1: $total")
    return total
end

function part2(sequences::Vector{SubString{String}})::Int
    total = 0
    for seq in sequences
        total += _get_first(seq)
    end
    
    println("Part 2: $total")
    return total
end

function main(session_value::String)
    cookies_header = "Cookie" => "session=$session_value"
    url = "https://adventofcode.com/2023/day/9/input"

    res = HTTP.get(url, headers=[cookies_header])
    raw_input = strip(String(res.body))

    sequences = split(raw_input, "\n")
    @time part1(sequences)
    @time part2(sequences)
end

main(ARGS[1])
