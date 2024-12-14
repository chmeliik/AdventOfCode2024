import re
from dataclasses import dataclass, replace
from itertools import groupby

import numpy as np
import numpy.typing as npt


@dataclass(frozen=True)
class Machine:
    button_a: npt.NDArray
    button_b: npt.NDArray
    prize: npt.NDArray


def as_equation(m: Machine) -> tuple[npt.NDArray, npt.NDArray]:
    return np.array([m.button_a, m.button_b]).transpose(), np.array(m.prize)


def token_cost(m: Machine) -> int:
    solution = np.linalg.solve(*as_equation(m))
    a = round(solution[0])
    b = round(solution[1])
    if (a * m.button_a + b * m.button_b == m.prize).all():
        return 3 * a + b
    else:
        return 0


def fix_prize(m: Machine) -> Machine:
    return replace(m, prize=m.prize + 10000000000000)


def parse_input(inp: str) -> list[Machine]:
    groups = [
        list(group)
        for isempty, group in groupby(inp.splitlines(), key=lambda line: not line)
        if not isempty
    ]

    def parse_line(line: str) -> npt.NDArray:
        m = re.search(r"(\d+)\D+(\d+)", line)
        assert m
        a, b = map(int, m.groups())
        return np.array([a, b])

    return [Machine(*map(parse_line, group)) for group in groups]


def part1(machines: list[Machine]) -> int:
    return sum(map(token_cost, machines))


def part2(machines: list[Machine]) -> int:
    return sum(map(token_cost, map(fix_prize, machines)))


def main() -> None:
    with open("input.txt") as f:
        inp = parse_input(f.read())

    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    main()
