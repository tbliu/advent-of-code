using HTTP
using JSON3

function part1(data::Vector{SubString{String}})::Int
    total = 0
    for line in data
        numbers = split(line, ": ")[2]  # Julia is 1-indexed
        winning_numbers_str, your_numbers_str = split(numbers, " | ")

        # We don't actually need to parse to int since the numbers themselves don't contribute to actual score
        winning_numbers = Set([num for num in split(winning_numbers_str)])
        num_matches = length([1 for num in split(your_numbers_str) if num in winning_numbers])

        if num_matches > 0
            total += 2^(num_matches-1)
        end

    end

    println("Part 1: $total")
    return total
end

function part2(data::Vector{SubString{String}})::Int
    # We have 1 copy of each card to start with and will never copy past the end of table
    cards_count = Dict(i => 1 for i in 1:length(data)) 

    for (i, line) in enumerate(data)
        numbers = split(line, ": ")[2]
        winning_numbers_str, your_numbers_str = split(numbers, " | ")

        winning_numbers = Set([num for num in split(winning_numbers_str)])
        num_matches = length([1 for num in split(your_numbers_str) if num in winning_numbers])
        
        if num_matches > 0
            multiplier = cards_count[i]
            start_bound = i + 1
            stop_bound = start_bound + (num_matches - 1)

            for j in start_bound:stop_bound
                cards_count[j] += multiplier  # multiplier * 1 === multiplier
            end
        end
    end

    total = 0
    for (card, count) in cards_count
        total += count
    end

    println("Part 2: $total")
    return total
end

function main(session_value::String)
    cookies_header = "Cookie" => "session=$session_value"
    url = "https://adventofcode.com/2023/day/4/input"

    res = HTTP.get(url, headers=[cookies_header])
    raw_input = String(res.body)
    data = split(strip(raw_input), "\n")

    @time part1(data)
    @time part2(data)
end

main(ARGS[1])
