import functools


def count_possible(designs: list[str], towels: list[str]) -> int:
    @functools.cache
    def is_possible(design: str) -> bool:
        if not design:
            return True
        return any(
            design.startswith(towel) and is_possible(design[len(towel) :]) for towel in towels
        )

    return sum(map(is_possible, designs))


def parse_input(inp: str) -> tuple[list[str], list[str]]:
    towels, _, *designs = inp.splitlines()
    return list(map(str.strip, towels.split(","))), designs


def part1(towels: list[str], designs: list[str]) -> int:
    return count_possible(designs, towels)


def main() -> None:
    with open("input.txt") as f:
        towels, designs = parse_input(f.read())

    print(part1(towels, designs))


if __name__ == "__main__":
    main()
