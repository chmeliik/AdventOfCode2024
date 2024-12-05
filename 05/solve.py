import graphlib
from itertools import pairwise
from collections import defaultdict
from typing import IO, Iterator

type Predecessors = dict[int, list[int]]
type Pages = list[int]


def parse_input(infile: IO[str]) -> tuple[Predecessors, list[Pages]]:
    preds: Predecessors = defaultdict(list)
    for line in iter(infile.readline, "\n"):
        a, b = map(int, line.split("|"))
        preds[b].append(a)

    updates: list[Pages] = [list(map(int, line.split(","))) for line in infile]
    return preds, updates


def rank_pages(pages: Pages, preds: Predecessors) -> dict[int, int]:
    graph = graphlib.TopologicalSorter()
    for page in pages:
        graph.add(page, *preds.get(page, []))
    graph.prepare()

    rank = {page: i for i, pages in enumerate(topo_iter(graph)) for page in pages}
    return rank


def topo_iter[T](graph: graphlib.TopologicalSorter[T]) -> Iterator[tuple[T, ...]]:
    while graph.is_active():
        yield (ready := graph.get_ready())
        graph.done(*ready)


def part1(preds: Predecessors, updates: list[Pages]) -> int:
    def are_in_correct_order(pages: Pages) -> bool:
        rank = rank_pages(pages, preds)
        return all(rank[b] >= rank[a] for a, b in pairwise(pages))

    correct_updates = filter(are_in_correct_order, updates)
    return sum(update[len(update) // 2] for update in correct_updates)


def part2(preds: Predecessors, updates: list[Pages]) -> int:
    def correct_order(pages: Pages) -> Pages:
        rank = rank_pages(pages, preds)
        return sorted(pages, key=lambda page: rank[page])

    corrected_updates = (fixed for pages in updates if (fixed := correct_order(pages)) != pages)
    return sum(update[len(update) // 2] for update in corrected_updates)


def main() -> None:
    with open("input.txt") as f:
        preds, updates = parse_input(f)

    print(part1(preds, updates))
    print(part2(preds, updates))


if __name__ == "__main__":
    main()
