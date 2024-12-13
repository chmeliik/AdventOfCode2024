import functools

type TopoMap = dict[tuple[int, int], int]


def parse_input(inp: str) -> TopoMap:
    return {
        (row, col): int(c)
        for row, line in enumerate(inp.splitlines())
        for col, c in enumerate(line)
    }


def surroundings(pos: tuple[int, int]) -> list[tuple[int, int]]:
    r, c = pos
    return [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]


def part1(topo_map: TopoMap) -> int:
    @functools.cache
    def reachable_nines(pos: tuple[int, int]) -> set[tuple[int, int]]:
        v = topo_map[pos]
        if v == 9:
            return {pos}
        return functools.reduce(
            set.union,
            (reachable_nines(p) for p in surroundings(pos) if topo_map.get(p) == v + 1),
            set(),
        )

    return sum(len(reachable_nines(pos)) for pos, v in topo_map.items() if v == 0)


def part2(topo_map: TopoMap) -> int:
    @functools.cache
    def surroundings_score(pos: tuple[int, int]) -> int:
        v = topo_map[pos]
        if v == 9:
            return 1
        return sum(surroundings_score(p) for p in surroundings(pos) if topo_map.get(p) == v + 1)

    return sum(surroundings_score(pos) for pos, v in topo_map.items() if v == 0)


def main() -> None:
    with open("input.txt") as f:
        inp = parse_input(f.read())

    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    main()
