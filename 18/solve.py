from collections import deque
from functools import partial
from typing import Callable, Iterable


type Pos = tuple[int, int]


def uncorrupted_neighbors_in_bounds(bounds: Pos, corrupted: set[Pos], pos: Pos) -> list[Pos]:
    x, y = pos
    bx, by = bounds
    neighbors = [
        (x2, y2)
        for x2, y2 in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        if 0 <= x2 <= bx
        if 0 <= y2 <= by
        if (x2, y2) not in corrupted
    ]
    return neighbors


def bfs(start: Pos, end: Pos, neighbors: Callable[[Pos], Iterable[Pos]]) -> int:
    queue = deque()
    queue.append(start)

    distance: dict[Pos, int] = {}
    distance[start] = 0

    while queue:
        current = queue.popleft()
        dist = distance[current]
        if current == end:
            return dist

        for neighbor in neighbors(current):
            if neighbor not in distance:
                distance[neighbor] = dist + 1
                queue.append(neighbor)

    raise ValueError(f"couldn't reach endpos {end}")


def parse_input(inp: str) -> list[Pos]:
    def parseline(line: str) -> Pos:
        a, b = map(int, line.split(","))
        return a, b

    return list(map(parseline, inp.splitlines()))


BOUNDS = (70, 70)


def part1(incoming_bytes: list[Pos]) -> int:
    corrupted = set(incoming_bytes[:1024])
    return bfs(
        (0, 0),
        BOUNDS,
        partial(uncorrupted_neighbors_in_bounds, BOUNDS, corrupted),
    )


def part2(incoming_bytes: list[Pos]) -> str:
    corrupted = set(incoming_bytes[:1024])

    for byte in incoming_bytes[1024:]:
        corrupted.add(byte)
        try:
            bfs(
                (0, 0),
                BOUNDS,
                partial(uncorrupted_neighbors_in_bounds, BOUNDS, corrupted),
            )
        except ValueError:
            return ",".join(map(str, byte))

    raise ValueError("path is never blocked")


def main() -> None:
    with open("input.txt") as f:
        incoming_bytes = parse_input(f.read())

    print(part1(incoming_bytes))
    print(part2(incoming_bytes))


if __name__ == "__main__":
    main()
