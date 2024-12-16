import enum
import heapq
import math
from typing import Iterator, NamedTuple


class Direction(enum.IntEnum):
    N = 0
    E = 1
    S = 2
    W = 3


def turn_right(d: Direction) -> Direction:
    return Direction((d + 1) % 4)


def turn_left(d: Direction) -> Direction:
    return Direction((d + 3) % 4)


type Pos = tuple[int, int]


def move(pos: Pos, d: Direction) -> Pos:
    x, y = pos
    match d:
        case Direction.N:
            return x - 1, y
        case Direction.S:
            return x + 1, y
        case Direction.W:
            return x, y - 1
        case Direction.E:
            return x, y + 1


class FacingPos(NamedTuple):
    pos: Pos
    direction: Direction


def possible_moves(p: FacingPos) -> list[FacingPos]:
    pos, d = p
    return [
        FacingPos(move(pos, d), d),
        FacingPos(pos, turn_left(d)),
        FacingPos(pos, turn_right(d)),
    ]


def cost(p1: FacingPos, p2: FacingPos) -> int:
    if p1.direction != p2.direction:
        return 1000
    return 1


type MazeWalls = set[Pos]
type Parents = dict[FacingPos, set[FacingPos]]


def cheapest_paths(walls: MazeWalls, start: FacingPos, end: Pos) -> tuple[int, Parents]:
    possible_parents: dict[FacingPos, set[FacingPos]] = {}
    distances: dict[FacingPos, int] = {start: 0}
    queue: list[tuple[int, FacingPos]] = [(0, start)]

    while queue:
        distance, current = heapq.heappop(queue)
        if current.pos == end:
            return distance, possible_parents

        for neighbor in possible_moves(current):
            if neighbor.pos in walls:
                continue

            new_neighbor_dist = distance + cost(current, neighbor)
            prev_neighbor_dist = distances.get(neighbor, math.inf)

            if new_neighbor_dist < prev_neighbor_dist:
                possible_parents[neighbor] = {current}
                distances[neighbor] = new_neighbor_dist
                heapq.heappush(queue, (new_neighbor_dist, neighbor))
            elif new_neighbor_dist == prev_neighbor_dist:
                possible_parents[neighbor].add(current)

    raise ValueError("couldn't reach endpos")


type Input = tuple[MazeWalls, FacingPos, Pos]


def parse_input(inp: str) -> Input:
    walls: MazeWalls = set()
    start: FacingPos | None = None
    end: Pos | None = None

    for x, line in enumerate(inp.splitlines()):
        for y, c in enumerate(line):
            if c == "#":
                walls.add((x, y))
            elif c == "S":
                start = FacingPos((x, y), Direction.E)
            elif c == "E":
                end = (x, y)

    assert start
    assert end

    return walls, start, end


def on_any_best_path(possible_parents: Parents, end: Pos) -> set[Pos]:
    def walk_backwards(fp: FacingPos) -> Iterator[Pos]:
        yield fp.pos
        for parent in possible_parents.get(fp, set()):
            yield from walk_backwards(parent)

    end_fp = next(fp for fp in possible_parents if fp.pos == end)
    return set(walk_backwards(end_fp))


def main() -> None:
    with open("input.txt") as f:
        maze, start, end = parse_input(f.read())

    price, possible_parents = cheapest_paths(maze, start, end)
    on_best_paths = on_any_best_path(possible_parents, end)

    print(price)
    print(len(on_best_paths))


if __name__ == "__main__":
    main()
