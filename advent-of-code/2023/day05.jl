using HTTP
using JSON3

function _convert_to_next(start_values::Vector{Int64}, map::SubString{String})::Vector{Int64}
    next_values = [num for num in start_values]
    map_values = split(map, "\n")[2:end]

    for row in map_values
        dest_start, src_start, range = [tryparse(Int64, x) for x in split(row)]
        for (i, value) in enumerate(start_values)
            src_end = src_start + range
            if value >= src_start && value < src_end
                offset = value - src_start
                new_val = dest_start + offset
                next_values[i] = new_val
            end
        end
    end

    return next_values
end

function part1(data::Vector{SubString{String}})::Int64
    seeds = [tryparse(Int64, num) for num in split(split(data[1], ": ")[2])]
    values = seeds

    for map in data[2:end]
        values = _convert_to_next(values, map) 
    end

    lowest = min(values...)
    println("Part 1: $values -> $lowest")
    return lowest
end


function _convert_to_next_with_intersection(start_values::Vector{Int64}, map::SubString{String})::Vector{Int64}
    next_values = []
    map_values = split(map, "\n")[2:end]  # Remove the header
    remaining_values = start_values

    for row in map_values
        dest_start, src_start, range = [tryparse(Int64, x) for x in split(row)]
        src_end = src_start + range
        next_remaining = []  # If any values are not intersected, see if we can intersect them with subsequent map values
        i = 1
        while i <= length(remaining_values)
            lower_bound = remaining_values[i]
            bound_range = remaining_values[i+1]
            upper_bound = lower_bound + bound_range

            # 1. No intersection - [ x x x ] { x x x }. Just propagate values forward
            # 2. If intersection, e.g.:
            #   a. [ x x { x x ] x x }
            #   b. { x x [ x x } x x ]
            #   c. [ x x { x x } x x ]
            #   d. { x x [ x x ] x x }
            # Intersected values are updated, remaining are split and propagated forward

            is_intersecting = lower_bound < src_end && upper_bound >= src_start
            if is_intersecting
                # Intersection range
                intersect_start = max(lower_bound, src_start)
                intersect_end = min(upper_bound, src_start + range)
                intersect_range = intersect_end - intersect_start
               
                # Update intersection bounds
                offset = intersect_start - src_start
                push!(next_values, dest_start + offset)
                push!(next_values, intersect_range)

                # Update any non-intersecting parts if needed. No updates necessary except the range value
                if lower_bound < intersect_start
                    push!(next_remaining, lower_bound)
                    push!(next_remaining, intersect_start - lower_bound)
                end

                if upper_bound > intersect_end
                    push!(next_remaining, intersect_end)
                    push!(next_remaining, upper_bound - intersect_end)
                end


            else
                push!(next_remaining, lower_bound)
                push!(next_remaining, bound_range)
            end
            
            i += 2
        end

        if length(next_remaining) === 0
            remaining_values = []
            break
        end
        remaining_values = next_remaining
    end

    return vcat(next_values, remaining_values)
end


function part2(data::Vector{SubString{String}})::Int64
    seeds = [tryparse(Int64, num) for num in split(split(data[1], ": ")[2])]
    values = seeds

    for (i, map) in enumerate(data[2:end])
        values = _convert_to_next_with_intersection(values, map)
    end

    # Since values includes ranges, only take the actual values
    # which will be in the odd indices (remember--julia is 1-indexed)
    lowest = min([v for (i, v) in enumerate(values) if i % 2 !== 0]...)

    println("Part 2: $values -> $lowest")
    return lowest
end

function main(session_value::String)
    cookies_header = "Cookie" => "session=$session_value"
    url = "https://adventofcode.com/2023/day/5/input"

    res = HTTP.get(url, headers=[cookies_header])
    raw_input = String(res.body)
    data = split(strip(raw_input), "\n\n")

    @time part1(data)
    @time part2(data)
end

main(ARGS[1])
