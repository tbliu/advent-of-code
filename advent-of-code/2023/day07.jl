#import Pkg; Pkg.add("DataStructures")

# NOTE: Some copypasta code here. To re-run, follow the NOTEs below

using HTTP
using JSON3
using DataStructures

# NOTE: remove the prefix for part 1
part1__CARD_ORDER = Dict(
                  '2' => 2,
                  '3' => 3,
                  '4' => 4,
                  '5' => 5,
                  '6' => 6,
                  '7' => 7,
                  '8' => 8,
                  '9' => 9,
                  'T' => 10,
                  'J' => 11,
                  'Q' => 12,
                  'K' => 13,
                  'A' => 14)

# Part 2 card order
CARD_ORDER = Dict(
                  'J' => 1,
                  '2' => 2,
                  '3' => 3,
                  '4' => 4,
                  '5' => 5,
                  '6' => 6,
                  '7' => 7,
                  '8' => 8,
                  '9' => 9,
                  'T' => 10,
                  'Q' => 12,
                  'K' => 13,
                  'A' => 14)


SCORES = Dict(
              "HIGH_CARD" => 0,
              "ONE_PAIR" => 1,
              "TWO_PAIR" => 2,
              "SET" => 3,
              "HOUSE" => 4,
              "FOUR_OF_A_KIND" => 5,
              "FIVE_OF_A_KIND" => 6)


# Heap stuff
struct HeapNode
    value::Int
    hand::SubString{String}
    cards::Vector{Pair{Int64, Char}}
    bid::Int
end

function Base.isless(a::HeapNode, b::HeapNode)
    if a.value !== b.value
        return a.value < b.value
    end

    for i in 1:length(a.hand)
        if a.hand[i] !== b.hand[i]
            return CARD_ORDER[a.hand[i]] < CARD_ORDER[b.hand[i]]
        end
    end

    for (A, B) in zip(a.cards, b.cards)
        a_count, a_card = A
        b_count, b_card = B
        if a_card != b_card
            return CARD_ORDER[a_card] < CARD_ORDER[b_card]
        end

        if a_count !== b_count
            return a_count < b_count 
        end

    end

    return false
end

function _compare_cards(a::Pair, b::Pair)::Bool
    """Compares cards of count => type, e.g. 3 => K vs 4 => Q"""
    # Sort this in reverse so we can get cards in desc. order
    if a[1] !== b[1]
        return a[1] > b[1]
    end

    a_card = a[2]
    b_card = b[2]
    if a_card != b_card
        return CARD_ORDER[a_card] > CARD_ORDER[b_card]
    end

    return false
end

# For part 1
function _get_hand(hand::SubString{String}, bid::Int)::HeapNode
    """Returns the rank up the hand type (high-card === 1, five-of-a-kind === 7)"""
    cards = Dict()
    for card in hand
        if !haskey(cards, card)
            cards[card] = 0
        end
        cards[card] += 1
    end
    
    card_count = []
    for (card, count) in cards
        push!(card_count, count => card)
    end

    card_count = sort(card_count, lt=_compare_cards)

    prev = SCORES["HIGH_CARD"]
    # This is in descending order, so we won't need to worry about
    # e.g. if we see a 3 of a kind, we haven't seen any pairs before then
    for (count, card) in card_count
        if count === 5
            # 5 of a kind
            return HeapNode(SCORES["FIVE_OF_A_KIND"], hand, card_count, bid)
        elseif count === 4
            # 4 of a kind
            return HeapNode(SCORES["FOUR_OF_A_KIND"], hand, card_count, bid)
        elseif count === 3
            if prev === SCORES["HIGH_CARD"]  # This is ok because in reality set will come before high cards
                prev = SCORES["SET"]
            end
        elseif count === 2
            if prev === SCORES["SET"]
                # full house
                return HeapNode(SCORES["HOUSE"], hand, card_count, bid)
            elseif prev == SCORES["ONE_PAIR"]
                # 2 pair
                return HeapNode(SCORES["TWO_PAIR"], hand, card_count, bid)
            else
                prev = max(prev, SCORES["ONE_PAIR"])
            end
        end
    end

    return HeapNode(prev, hand, card_count, bid)  # high card
end

# For part 2
function _get_hand2(hand::SubString{String}, bid::Int)::HeapNode
    """Returns the rank up the hand type (high-card === 1, five-of-a-kind === 7)"""
    cards = Dict()
    for card in hand
        if !haskey(cards, card)
            cards[card] = 0
        end
        cards[card] += 1
    end
    
    card_count = []
    jokers = 0
    for (card, count) in cards
        if card === 'J'
            jokers += count
            continue
        end
        push!(card_count, count => card)
    end

    card_count = sort(card_count, lt=_compare_cards)

    prev = SCORES["HIGH_CARD"]
    node = nothing

    # This is in descending order, so we won't need to worry about
    # e.g. if we see a 3 of a kind, we haven't seen any pairs before then
    for (count, card) in card_count
        if count === 5
            # 5 of a kind
            node = HeapNode(SCORES["FIVE_OF_A_KIND"], hand, card_count, bid)
            break
        elseif count === 4
            # 4 of a kind
            node = HeapNode(SCORES["FOUR_OF_A_KIND"], hand, card_count, bid)
            break
        elseif count === 3
            if prev === SCORES["HIGH_CARD"]  # This is ok because in reality set will come before high cards
                prev = SCORES["SET"]
            end
        elseif count === 2
            if prev === SCORES["SET"]
                # full house
                node = HeapNode(SCORES["HOUSE"], hand, card_count, bid)
                break
            elseif prev == SCORES["ONE_PAIR"]
                # 2 pair
                node = HeapNode(SCORES["TWO_PAIR"], hand, card_count, bid)
                break
            else
                prev = max(prev, SCORES["ONE_PAIR"])
            end
        end
    end


    if node === nothing
        node = HeapNode(prev, hand, card_count, bid)
    end

    # Got the best hand without jokers, now add jokers in.
    # It's always beneficial to add jokers to the highest card already there
    if jokers === 5 || jokers === 4
        return HeapNode(SCORES["FIVE_OF_A_KIND"], hand, card_count, bid)
    elseif jokers === 3
        if node.value === SCORES["ONE_PAIR"]
            return HeapNode(SCORES["FIVE_OF_A_KIND"], hand, card_count, bid)
        else
            return HeapNode(SCORES["FOUR_OF_A_KIND"], hand, card_count, bid)
        end
    elseif jokers === 2
        if node.value === SCORES["ONE_PAIR"]
            return HeapNode(SCORES["FOUR_OF_A_KIND"], hand, card_count, bid)
        elseif node.value === SCORES["SET"]
            return HeapNode(SCORES["FIVE_OF_A_KIND"], hand, card_count, bid)
        else
            return HeapNode(SCORES["SET"], hand, card_count, bid)
        end
    elseif jokers === 1
        if node.value === SCORES["ONE_PAIR"]
            return HeapNode(SCORES["SET"], hand, card_count, bid)
        elseif node.value === SCORES["TWO_PAIR"]
            return HeapNode(SCORES["HOUSE"], hand, card_count, bid)
        elseif node.value === SCORES["SET"]
            return HeapNode(SCORES["FOUR_OF_A_KIND"], hand, card_count, bid)
        elseif node.value === SCORES["FOUR_OF_A_KIND"]
            return HeapNode(SCORES["FIVE_OF_A_KIND"], hand, card_count, bid)
        else
            return HeapNode(SCORES["ONE_PAIR"], hand, card_count, bid)
        end
    else
        return node
    end
end

function part1(data::Vector{SubString{String}})::Int
    heap = BinaryMinHeap{HeapNode}()

    for line in data
        hand, bid_str = split(line, " ")
        bid = tryparse(Int64, bid_str)
        push!(heap, _get_hand(hand, bid))
    end

    rank = 1
    total = 0
    while !isempty(heap)
        node = pop!(heap)
        total += rank * node.bid
        rank += 1
    end

    println("Part 1: $total")
    return total
end


function part2(data::Vector{SubString{String}})::Int
    heap = BinaryMinHeap{HeapNode}()

    for line in data
        hand, bid_str = split(line, " ")
        bid = tryparse(Int64, bid_str)
        push!(heap, _get_hand2(hand,bid))
    end

    rank = 1
    total = 0
    while !isempty(heap)
        node = pop!(heap)
        total += rank * node.bid
        rank += 1
    end

    println("Part 2: $total")
    return total
end

function main(session_value::String)
    cookies_header = "Cookie" => "session=$session_value"
    url = "https://adventofcode.com/2023/day/7/input"

    res = HTTP.get(url, headers=[cookies_header])
    raw_input = String(res.body)
    data = split(strip(raw_input), "\n")

    # NOTE: Uncomment this line, and comment out the part 2 invocation
    #@time part1(data)
    @time part2(data)
end

main(ARGS[1])

