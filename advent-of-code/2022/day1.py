from typing import List

import click
import heapq
import requests

def part1(data: List[str]):
    max_calories = float('-inf')
    elf_calories = 0

    for row in data:
        if row.strip() == '':
            max_calories = max(max_calories, elf_calories)
            elf_calories = 0
        else:
            elf_calories += int(row.strip())

    print('part1 solution: ', max_calories)
    return max_calories


def part2(data: List[str]):
    heap = []
    calories = 0

    for row in data:
        if row.strip() == '':
            heapq.heappush(heap, calories)
            calories = 0
        else:
            calories += int(row.strip())
    top_3 = sum(heapq.nlargest(n=3, iterable=heap))
    print('part2 solution: ', top_3)
    return top_3


@click.command()
@click.option('--cookie')
def main(cookie: str):
    r = requests.get(
        url='https://adventofcode.com/2022/day/1/input',
        cookies={'session': cookie},
    )

    data = r.content.decode('utf-8').split('\n')
    part1(data)
    part2(data)


if __name__ == '__main__':
    main()
