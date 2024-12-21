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


def possible_cheats(walls: MazeWalls, pos: Pos, max_distance: int = 2) -> list[Pos]:
    x, y = pos
    return [
        newpos
        for x2 in range(x - max_distance, x + max_distance + 1)
        for y2 in range(y - max_distance, y + max_distance + 1)
        if 2 <= manhattan_dist(pos, newpos := (x2, y2)) <= max_distance
        if newpos not in walls
    ]


def manhattan_dist(p1: Pos, p2: Pos) -> int:
    x1, y1 = p1
    x2, y2 = p2
    return abs(x2 - x1) + abs(y2 - y1)


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


def count_cheats_that_save_at_least(
    picoseconds: int,
    max_cheat_length: int,
    maze: MazeWalls,
    distances_from_end: dict[Pos, int],
    normal_path_to_end: Iterable[Pos],
) -> int:
    inf = float("inf")

    def saves_at_least(pos: Pos, cheat: Pos, picoseconds: int) -> bool:
        return (
            distances_from_end[pos]
            - distances_from_end.get(cheat, inf)
            - manhattan_dist(pos, cheat)
            >= picoseconds
        )

    return sum(
        saves_at_least(pos, cheat, picoseconds)
        for pos in normal_path_to_end
        for cheat in possible_cheats(maze, pos, max_cheat_length)
    )


def part1(maze: MazeWalls, start: Pos, end: Pos) -> int:
    distance, parents = bfs(end, start, partial(possible_moves, maze))
    return count_cheats_that_save_at_least(100, 2, maze, distance, walk_backwards(parents, start))


def part2(maze: MazeWalls, start: Pos, end: Pos) -> int:
    distance, parents = bfs(end, start, partial(possible_moves, maze))
    return count_cheats_that_save_at_least(100, 20, maze, distance, walk_backwards(parents, start))

def main() -> None:
    with open("input.txt") as f:
        maze, start, end = parse_input(f.read())

    print(part1(maze, start, end))
    print(part2(maze, start, end))


if __name__ == "__main__":
    main()
