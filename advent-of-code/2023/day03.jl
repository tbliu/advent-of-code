# import Pkg; Pkg.add("HTTP"); Pkg.add("JSON3")

using HTTP
using JSON3


function _is_symbol(data::Vector{SubString{String}}, row::Int, col::Int)::Bool
    if row <= 0 || row >= length(data) + 1
        return false
    end

    if col <= 0 || col >= length(data[row]) + 1
        return false
    end

    if data[row][col] === '.' || isdigit(data[row][col])
        return false
    end

    return true
end    

function part1(data::Vector{SubString{String}})::Int
    total = 0
    for (row, line) in enumerate(data)
        col = 1
        while col <= length(line)
            if !isdigit(line[col])
                col += 1
                continue
            end

            num_end = col
            is_symbol = false
            while num_end <= length(line) && isdigit(line[num_end])
                # If any digit in the number is a part-symbol, then the whole number is
                if !is_symbol
                    is_symbol = is_symbol || _is_symbol(data, row+1, num_end)
                    is_symbol = is_symbol || _is_symbol(data, row-1, num_end)
                    is_symbol = is_symbol || _is_symbol(data, row, num_end+1)
                    is_symbol = is_symbol || _is_symbol(data, row, num_end-1)
                    is_symbol = is_symbol || _is_symbol(data, row-1, num_end-1)
                    is_symbol = is_symbol || _is_symbol(data, row+1, num_end-1)
                    is_symbol = is_symbol || _is_symbol(data, row-1, num_end+1)
                    is_symbol = is_symbol || _is_symbol(data, row+1, num_end+1)
                end
              
                num_end += 1
            end

            number = tryparse(Int64, line[col:num_end-1])

            if is_symbol
                total += number
            end

            col = num_end
        end
    end

    println("Part 1: $total")
    return total
end


function _expand_numbers(data::Vector{SubString{String}}, row::Int, col::Int, same_row::Bool)::Vector{Int}
    if row <= 0 || row > length(data)
        return []
    end

    if col <= 0 || col > length(data[row])
        return []
    end
 
    prefix = ""
    suffix = ""

    line = data[row]
 
    p = col - 1
    while p > 0 && isdigit(line[p])
        prefix = line[p] * prefix
        p -= 1
    end

    s = col + 1
    while s <= length(line) && isdigit(line[s])
        suffix = suffix * line[s]
        s += 1
    end

    # If line[col] is a digit, then coalesce it with the prefix and suffix
    # If it's not a digit, but we are looking at the same row as "*" then there is necessarily a gap between any numbers we find and the "*"
    # If it's not a digit, but we are looking at the row above or below the "*", then we treat prefix and suffix as two separate numbers
    if isdigit(line[col])
        number = tryparse(Int64, prefix * line[col] * suffix)
        return [number]
    elseif same_row
        return []
    else
        maybe_prefix = tryparse(Int64, prefix)
        maybe_suffix = tryparse(Int64, suffix)
        ret = []
        if maybe_prefix !== nothing
            push!(ret, maybe_prefix)
        end

        if maybe_suffix !== nothing
            push!(ret, maybe_suffix)
        end

        return ret
    end
end    

function part2(data::Vector{SubString{String}})::Int
    total = 0
    for (row, line) in enumerate(data)
        col = 1
        while col <= length(line)
            if line[col] !== '*'
                col += 1
                continue
            end

            top = _expand_numbers(data, row-1, col, false)
            bottom = _expand_numbers(data, row+1, col, false)
            left = _expand_numbers(data, row, col-1, true)
            right = _expand_numbers(data, row, col+1, true)

            num_adjacent = length(top) + length(bottom) + length(left) + length(right)
            if num_adjacent !== 2
                col += 1
                continue
            else
                total += (isempty(top) ? 1 : prod(top)) *
                    (isempty(bottom) ? 1 : prod(bottom)) *
                    (isempty(left) ? 1 : prod(left)) *
                    (isempty(right) ? 1 : prod(right))
                col += 1
            end
        end
    end

    println("Part 2: $total")
    return total
end


function main(session_value::String)
    cookies_header = "Cookie" => "session=$session_value"
    url = "https://adventofcode.com/2023/day/3/input"

    res = HTTP.get(url, headers=[cookies_header])
    raw_input = String(res.body)
    data = split(strip(raw_input), "\n")

    part1(data)
    part2(data)

end

# Expects session cookie as argument in command line invokation.
# Usage: julia day03.jl <session_cookie>
main(ARGS[1])
