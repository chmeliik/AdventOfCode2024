import re
from itertools import groupby, pairwise
from typing import cast

type Input = list[tuple[int, int] | bool]


def parse_input(inp: str) -> Input:
    mul = r"mul\((?P<a>\d{1,3}),(?P<b>\d{1,3})\)"
    do_or_dont = r"(?P<do_or_dont>do(n't)?\(\))"
    pat = re.compile(rf"(?:{mul})|(?:{do_or_dont})")

    def process_match(m: re.Match) -> tuple[int, int] | bool:
        groups = m.groupdict()
        if groups["a"]:
            return int(groups["a"]), int(groups["b"])
        else:
            return groups["do_or_dont"] == "do()"

    return list(map(process_match, pat.finditer(inp)))


def part1(inp: Input) -> int:
    return sum(x[0] * x[1] for x in inp if isinstance(x, tuple))


def part2(inp: Input) -> int:
    inp = [True] + inp
    grouped = groupby(inp, lambda x: isinstance(x, bool))

    def handle_pair(conditionals: list[bool], muls: list[tuple[int, int]]) -> int:
        if conditionals[-1]:
            return sum(a * b for a, b in muls)
        else:
            return 0

    result = 0

    for maybe_conditionals, maybe_muls in pairwise(map(list, (v for _, v in grouped))):
        if isinstance(maybe_conditionals[0], bool):
            result += handle_pair(
                cast(list[bool], maybe_conditionals),
                cast(list[tuple[int, int]], maybe_muls),
            )

    return result


def main() -> None:
    with open("input.txt") as f:
        inp = parse_input(f.read())

    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    main()
