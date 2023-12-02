from typing import List

import click
import re
import requests


def part1(data: List[str]):
    total = 0
    for line in data:
        i = 0
        j = len(line) - 1
        first = None
        second = None

        while i <= j:
            if line[i].isdigit() and first is None:
                first = line[i]
            if line[j].isdigit() and second is None:
                second = line[j]
            if first is not None and second is not None:
                break

            # Otherwise the loop condition fails before one of the numbers is set
            if first is None:
                i += 1
            if second is None:
                j -= 1

        total += int(first + second)

    print(f'Part 1: {total}')
    return total


def part2(data: List[str]):
    total = 0
    for original_line in data:
        # Twone should be 'two' and 'one'
        line = original_line.replace('one', 'o1ne')
        line = line.replace('two', 't2wo')
        line = line.replace('three', 'th3ree')
        line = line.replace('four', 'fo4ur')
        line = line.replace('five', 'fi5ve')
        line = line.replace('six', 's6ix')
        line = line.replace('seven', 'sev7en')
        line = line.replace('eight', 'eig8ht')
        line = line.replace('nine', 'ni9ne')
        
        matches = re.findall('\d', line)
        first = matches[0]
        if not first.isdigit():
            first = str(digit_map.index(first))
            
        second = matches[-1]
        if not second.isdigit():
            second = str(digit_map.index(second))

        print(line, original_line, first, second, int(first+second))
        total += int(first + second)

    print(f'Part 2: {total}')
    return total


@click.command()
@click.option('--cookie')
def main(cookie: str):
    r = requests.get(
        url='https://adventofcode.com/2023/day/1/input',
        cookies={'session': cookie},
    )

    data = r.content.decode('utf-8').strip().split('\n')
    part1(data)
    part2(data)


if __name__ == '__main__':
    main()
