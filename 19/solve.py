import functools


def count_ways_to_make(designs: list[str], towels: list[str]) -> list[int]:
    @functools.cache
    def ways_to_make(design: str) -> int:
        if not design:
            return 1
        return sum(
            ways_to_make(design[len(towel) :]) for towel in towels if design.startswith(towel)
        )

    return list(map(ways_to_make, designs))


def parse_input(inp: str) -> tuple[list[str], list[str]]:
    towels, _, *designs = inp.splitlines()
    return list(map(str.strip, towels.split(","))), designs


def main() -> None:
    with open("input.txt") as f:
        towels, designs = parse_input(f.read())

    ways_to_make = count_ways_to_make(designs, towels)
    print(sum(n > 0 for n in ways_to_make))
    print(sum(ways_to_make))


if __name__ == "__main__":
    main()
