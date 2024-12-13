from itertools import groupby
from typing import Iterable, Literal

type Pos = tuple[int, int]
type Garden = dict[Pos, str]


def surroundings(pos: Pos) -> list[Pos]:
    r, c = pos
    return [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]


def split_to_regions(garden: Garden) -> list[set[Pos]]:
    region_map: dict[Pos, int] = {}

    def _mark_region(pos: Pos, mark: int) -> None:
        if pos in region_map:
            return
        region_map[pos] = mark
        v = garden[pos]
        for p in surroundings(pos):
            if garden.get(p) == v:
                _mark_region(p, mark)

    mark = 0
    for pos in garden:
        _mark_region(pos, mark)
        mark += 1

    sorted_by_region = sorted(region_map, key=lambda pos: region_map[pos])
    grouped_by_region = groupby(sorted_by_region, key=lambda pos: region_map[pos])

    return [set(region) for _, region in grouped_by_region]


type Direction = Literal["N", "E", "S", "W"]
type DirectionalPos = tuple[Pos, Direction]


def directional_surroundings(pos: Pos) -> Iterable[DirectionalPos]:
    return zip(surroundings(pos), ["S", "N", "E", "W"])


def perimeter(region: set[Pos]) -> set[DirectionalPos]:
    return {
        (pos, direction)
        for pos in region
        for (surrounding_pos, direction) in directional_surroundings(pos)
        if surrounding_pos not in region
    }


def count_sides(perimeter: set[DirectionalPos]) -> int:
    part_of_some_side: set[DirectionalPos] = set()

    def follow_whole_side(dpos: DirectionalPos) -> None:
        if dpos in part_of_some_side:
            return
        part_of_some_side.add(dpos)
        pos, direction = dpos
        for p in surroundings(pos):
            if (p, direction) in perimeter:
                follow_whole_side((p, direction))

    n_sides = 0
    for dpos in perimeter:
        if dpos not in part_of_some_side:
            follow_whole_side(dpos)
            n_sides += 1

    return n_sides


def parse_input(inp: str) -> Garden:
    return {
        (row, col): char
        for row, line in enumerate(inp.splitlines())
        for col, char in enumerate(line)
    }


def part1(garden: Garden) -> int:
    regions = split_to_regions(garden)
    return sum(len(region) * len(perimeter(region)) for region in regions)


def part2(garden: Garden) -> int:
    regions = split_to_regions(garden)
    return sum(len(region) * count_sides(perimeter(region)) for region in regions)


def main() -> None:
    with open("input.txt") as f:
        inp = parse_input(f.read())

    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    main()
