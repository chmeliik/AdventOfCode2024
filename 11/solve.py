import functools
from typing import Iterable


def rule(n: int) -> tuple[int, ...]:
    if n == 0:
        return (1,)
    d, m = divmod(n_digits(n), 2)
    if m == 0:
        return divmod(n, 10**d)
    else:
        return (n * 2024,)


def n_digits(a: int) -> int:
    n = 0
    while a > 0:
        n += 1
        a //= 10
    return n


@functools.cache
def length_after_n_steps(steps: int, num: int) -> int:
    if steps == 0:
        return 1
    return sum_length_after_n_steps(steps - 1, rule(num))


def sum_length_after_n_steps(steps: int, nums: Iterable[int]) -> int:
    return sum(length_after_n_steps(steps, num) for num in nums)


def parse_input(inp: str) -> list[int]:
    return list(map(int, inp.split()))


def part1(inp: list[int]) -> int:
    return sum_length_after_n_steps(25, inp)


def part2(inp: list[int]) -> int:
    return sum_length_after_n_steps(75, inp)


def main() -> None:
    with open("input.txt") as f:
        inp = parse_input(f.read())

    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    main()
