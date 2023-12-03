# import Pkg; Pkg.add("HTTP"); Pkg.add("JSON3")

using HTTP
using JSON3


function part01(data::Vector{SubString{String}})::Int
    total = 0
    for (row, line) in enumerate(data)
        col = 1
        while col < length(line)
            if !isdigit(line[col])
                col += 1
                continue
            end

            num_end = col + 1
            while num_end < length(line) && isdigit(line[num_end])
                num_end += 1
            end

            number = tryparse(Int64, line[col:num_end-1])
            println(number, " ", line[col:num_end-1])

            col = num_end
        end
    end

    return total
end


function main(session_value::String)
    cookies_header = "Cookie" => "session=$session_value"
    url = "https://adventofcode.com/2023/day/3/input"

    res = HTTP.get(url, headers=[cookies_header])
    raw_input = String(res.body)
    data = split(strip(raw_input), "\n")

    part01(data)

end

# Expects session cookie as argument in command line invokation.
# Usage: julia day03.jl <session_cookie>
main(ARGS[1])
     
    
