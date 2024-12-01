from typing import List

import click
import re
import requests

def part1(data: List[str]):
    total = 0
    RED = 12
    GREEN = 13
    BLUE = 14

    for i, line in enumerate(data):
        game_id = i + 1
        game = line.split(': ')[1]  # Strip the 'Game X: ' prefix
        draws = game.split('; ')
        is_game_valid = True
        for draw in draws:
            results = draw.split(', ')
            for result in results:
                number = int(''.join(re.findall('\d', result)))
                if 'green' in result and number <= GREEN:
                    pass
                elif 'red' in result and number <= RED:
                    pass
                elif 'blue' in result and number <= BLUE:
                    pass
                else:
                    is_game_valid = False
                    break
            
            if not is_game_valid:
                break

        if is_game_valid:
            total += game_id

    print(f'Part 1: {total}')
    return total
        

def part2(data: List[str]):
    total = 0

    for line in data:
        red, green, blue = 0, 0, 0
        game = line.split(': ')[1]
        draws = game.split('; ')
        for draw in draws:
            results = draw.split(', ')
            for result in results:
                number = int(''.join(re.findall('\d', result)))
                if 'green' in result:
                    green = max(green, number)
                elif 'red' in result:
                    red = max(red, number)
                elif 'blue' in result:
                    blue = max(blue, number)
        total += red * green * blue

    print(f'Part 2: {total}')
    return total


@click.command()
@click.option('--cookie')
def main(cookie: str):
    r = requests.get(
        url='https://adventofcode.com/2023/day/2/input',
        cookies={'session': cookie},
    )

    data = r.content.decode('utf-8').strip().split('\n')
    part1(data)
    part2(data)


if __name__ == '__main__':
    main()
