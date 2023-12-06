using HTTP
using JSON3

# Idea is to run binary search twice to find the least and most time 
# you can hold down while still beating the distance record
function _binary_search(time::Int, distance_record::Int, find_top::Bool)::Int
    lower_bound = 0
    upper_bound = time
    mid = -1

    while lower_bound < upper_bound
        mid = div((lower_bound + upper_bound), 2)
        distance = mid * (time - mid)

        if distance > distance_record
            if find_top
                lower_bound = mid+1
            else
                upper_bound = mid-1
            end
        else
            if find_top
                upper_bound = mid-1
            else
                lower_bound = mid+1
            end
        end
    end

    # Edge cases: Sometimes are bounds are just outside of the actual zone of winning times
    # so we just offset by 1 as needed
    if find_top
        if (time - upper_bound)*upper_bound <= distance_record
            return upper_bound - 1
        else
            return upper_bound
        end
    else
        if (time - lower_bound)*lower_bound <= distance_record
            return lower_bound + 1
        else
            return lower_bound
        end
    end
    return mid
end

function _solve(time::Vector{SubString{String}}, distance::Vector{SubString{String}})::Int
    total = 0
    for (t, d) in zip(time, distance)
        t = tryparse(Int64, t)
        d = tryparse(Int64, d)

        upper = _binary_search(t, d, true)
        lower = _binary_search(t, d, false)

        num_ways = upper - lower + 1

        if total === 0
            total = num_ways
        else
            total = total * num_ways
        end
    end
    
    return total
end


# Parts 1 and 2 share the same core implementation :-)
# We just need to format appropriately (strip spaces for part 2 and put it into a 1-length vector)

function part1(input::String)::Int
    time, distance = split(input, "\n")
    time = split(strip(split(time, ":")[2]))
    distance = split(strip(split(distance, ":")[2]))

    total = _solve(time, distance)    
    println("Part 1: $total")
    return total
end

function part2(input::String)::Int
    time, distance = split(input, "\n")
    time = split(replace(split(time, ":")[2], " " => ""))
    distance = split(replace(strip(split(distance, ":")[2]), " " => ""))

    total = _solve(time, distance)
    println("Part 2: $total")
    return total
end

function main(session_value::String)
    cookies_header = "Cookie" => "session=$session_value"
    url = "https://adventofcode.com/2023/day/6/input"

    res = HTTP.get(url, headers=[cookies_header])
    data = String(res.body)

    @time part1(data)
    @time part2(data)
end

main(ARGS[1])

