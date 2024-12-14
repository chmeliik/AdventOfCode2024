import operator as op
from collections import Counter
from functools import partial, reduce
from pathlib import Path
from typing import Any

import numpy as np
import numpy.typing as npt
from PIL import Image

type Vec = npt.NDArray[np.signedinteger[Any]]
type Robot = tuple[Vec, Vec]

# width, height
DIMENSIONS: Vec = np.array([101, 103])


def move(t: int, robot: Robot) -> Vec:
    p, v = robot
    return (p + t * v) % DIMENSIONS


def move_all(t: int, robots: list[Robot]) -> list[Vec]:
    return list(map(partial(move, t), robots))


def quadrant(p: Vec) -> int | None:
    middle = DIMENSIONS // 2
    if (p == middle).any():
        return None

    bits = p < middle
    return bits[0] + 2 * bits[1]


def interestingness(robots: list[Vec]) -> int:
    # "interesting" = the robots are closely clustered around the middle line
    # let's hope the tree picture will be in the middle
    middle_line = DIMENSIONS[0] // 2
    return sum(abs(middle_line - r[0]) for r in robots)


def save_picture(robots: list[Vec], path: Path) -> None:
    space = np.zeros((DIMENSIONS[1], DIMENSIONS[0], 3), dtype=np.uint8)
    for robot in robots:
        # make robots green
        space[robot[1], robot[0], 1] = 255

    img = Image.fromarray(space, "RGB")
    img.save(path)


def parse_input(inp: str) -> list[Robot]:
    def parse_vector(s: str) -> Vec:
        _, _, nums = s.partition("=")
        return np.array(list(map(int, nums.split(","))))

    def parse_line(line: str) -> Robot:
        p, v = line.split()
        return parse_vector(p), parse_vector(v)

    return list(map(parse_line, inp.splitlines()))


def part1(robots: list[Robot]) -> int:
    moved = move_all(100, robots)
    quadrants = filter(lambda x: x is not None, map(quadrant, moved))
    quadrant_counts = Counter(quadrants)
    return reduce(op.mul, quadrant_counts.values())


def part2(robots: list[Robot]) -> None:
    # generate the first 10_000 states, sort by "interestingness"
    states = [(t, move_all(t, robots)) for t in range(10_000)]
    states.sort(key=lambda x: interestingness(x[1]))

    pictures = Path("pictures")
    pictures.mkdir(exist_ok=True)
    # save the pictures for the 100 most interesting states
    for t, state in states[:100]:
        save_picture(state, pictures / f"{t}.png")


def main() -> None:
    with open("input.txt") as f:
        inp = parse_input(f.read())

    print(part1(inp))
    part2(inp)


if __name__ == "__main__":
    main()
