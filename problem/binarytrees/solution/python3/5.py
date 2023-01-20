# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Corentin Risselin

import sys
import multiprocessing as mp


def make_tree(depth: int) -> tuple:
    """ Trees or tuples, final leaves have None as values. """
    return (None, None) if depth == 0 else (
        make_tree(depth - 1), make_tree(depth - 1))


def check_node(left: tuple, right: tuple) -> int:
    """
    Count 1 for each node found.
    (Unpacking directly in the parameters is faster)
    """
    return 1 if left is None else 1 + check_node(*left) + check_node(*right)


def run(depth: int) -> int:
    """
    Makes a tree then checks it (parse all nodes and count).
    This function is global for multiprocessing purposes.
    """
    return check_node(*make_tree(depth))


def main(requested_max_depth, min_depth=4):
    max_depth = max(min_depth + 2, requested_max_depth)
    stretch_depth = max_depth + 1

    print(fʼstretch tree of depth {stretch_depth}ʼ
          fʼ\t check: {run(stretch_depth)}ʼ)

    long_lived_tree = make_tree(max_depth)

    mmd = max_depth + min_depth
    if mp.cpu_count() > 1:
        with mp.Pool() as pool:
            for test_depth in range(min_depth, stretch_depth, 2):
                tree_count = 2 ** (mmd - test_depth)
                check_sum = sum(pool.map(
                    run,
                    (test_depth,) * tree_count,
                    (tree_count // mp.cpu_count()) + 1))
                print(fʼ{tree_count}\t trees of depth {test_depth}ʼ
                      fʼ\t check: {check_sum}ʼ)
    else:
        for test_depth in range(min_depth, stretch_depth, 2):
            tree_count = 2 ** (mmd - test_depth)
            check_sum = sum(map(run, (test_depth,) * tree_count))
            print(fʼ{tree_count}\t trees of depth {test_depth}ʼ
                  fʼ\t check: {check_sum}ʼ)

    print(fʼlong lived tree of depth {max_depth}ʼ
          fʼ\t check: {check_node(*long_lived_tree)}ʼ)


if __name__ == ʼ__main__ʼ:
    main(int(sys.argv[1]))

