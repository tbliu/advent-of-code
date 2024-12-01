using HTTP
using JSON3

function _step_until(directions, maze_map, start)::Int
    count = 0
    curr = start
    i = 1
    while !endswith(curr, 'Z')
        direction = directions[i]
        candidates = maze_map[curr]
        if direction === 'L'
            curr = candidates[1]
        else
            curr = candidates[2]
        end
        count += 1
        i += 1
        if i > length(directions)
            i = 1
        end
    end
    return count
end


function part1(directions::SubString{String}, maze_map)::Int
    curr = "AAA"
    count = _step_until(directions, maze_map, "AAA")

    println("Part 1: $count")
    return count
end


function part2(directions::SubString{String}, maze_map)::Int
    starts = []
    for (elem, _) in maze_map
        if endswith(elem, 'A')
            push!(starts, elem)
        end
    end
    counts = [_step_until(directions, maze_map, start) for start in starts]
    count = lcm(counts)
   
    println("Part 2: $count")
    return count
end


function main(session_value::String)
    cookies_header = "Cookie" => "session=$session_value"
    url = "https://adventofcode.com/2023/day/8/input"

    res = HTTP.get(url, headers=[cookies_header])
    raw_input = strip(String(res.body))

    directions, maze = split(raw_input, "\n\n")
    maze = split(maze, "\n")
    maze_map = Dict()
    for line in maze
        pattern = r"[A-Z]+"
        elem, left, right = [m.match for m in eachmatch(pattern, line)]
        maze_map[elem] = (left => right)
    end

    @time part1(directions, maze_map)
    @time part2(directions, maze_map)
end

main(ARGS[1])
