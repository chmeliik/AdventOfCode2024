from collections import deque
from functools import partial
from typing import Callable, Iterable, Iterator

type Pos = tuple[int, int]
type MazeWalls = set[Pos]


type Input = tuple[MazeWalls, Pos, Pos]


def neighbors(pos: Pos) -> list[Pos]:
    x, y = pos
    return [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]


def possible_moves(walls: MazeWalls, pos: Pos) -> list[Pos]:
    return [neighbor for neighbor in neighbors(pos) if neighbor not in walls]


def possible_cheats(walls: MazeWalls, pos: Pos) -> set[Pos]:
    return {
        step2_neighbor
        for neighbor in neighbors(pos)
        for step2_neighbor in neighbors(neighbor)
        if step2_neighbor != pos
        if step2_neighbor not in walls
    }


def bfs(
    start: Pos, end: Pos, get_neighbors: Callable[[Pos], Iterable[Pos]]
) -> tuple[dict[Pos, int], dict[Pos, Pos]]:
    queue = deque()
    queue.append(start)

    distance: dict[Pos, int] = {}
    distance[start] = 0

    parents: dict[Pos, Pos] = {}
    parents = {}

    while queue:
        current = queue.popleft()
        if current == end:
            return distance, parents

        dist = distance[current]

        for neighbor in get_neighbors(current):
            if neighbor not in distance:
                distance[neighbor] = dist + 1
                parents[neighbor] = current
                queue.append(neighbor)

    raise ValueError(f"couldn't reach endpos {end}")


def walk_backwards(parents: dict[Pos, Pos], end: Pos) -> Iterator[Pos]:
    current = end
    while current:
        yield current
        current = parents.get(current)


def parse_input(inp: str) -> Input:
    walls: MazeWalls = set()
    start: Pos | None = None
    end: Pos | None = None

    for x, line in enumerate(inp.splitlines()):
        for y, c in enumerate(line):
            if c == "#":
                walls.add((x, y))
            elif c == "S":
                start = (x, y)
            elif c == "E":
                end = (x, y)

    assert start
    assert end

    return walls, start, end


def part1(maze: MazeWalls, start: Pos, end: Pos) -> int:
    distance, parents = bfs(end, start, partial(possible_moves, maze))
    inf = float("inf")
    return sum(
        distance[pos] - distance.get(cheat, inf) - 2 >= 100
        for pos in walk_backwards(parents, start)
        for cheat in possible_cheats(maze, pos)
    )


def main() -> None:
    with open("input.txt") as f:
        maze, start, end = parse_input(f.read())

    print(part1(maze, start, end))


if __name__ == "__main__":
    main()
