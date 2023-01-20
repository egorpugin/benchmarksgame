/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Danial Klimkin (C++)
 * contributed by the Rust Project Developers (Rust)
 * contributed by TeXitoi (Rust)
 * contributed by Cristi Cobzarenco (Rust)
 * contributed by Matt Brubeck (Rust)
 * contributed by Dmytro Ovdiienko
 * contributed by Martin Jambrek
 *
 */

#include <algorithm>
#include <execution>
#include <iostream>
#include <memory_resource>
#include <numeric>
#include <boost/iterator/counting_iterator.hpp>

using MemoryPool = std::pmr::monotonic_buffer_resource;

struct Node {
    Node *l, *r;

    int check() const
    {
        if (l)
            return l->check() + 1 + r->check();
        else
            return 1;
    }
};

inline static Node* make(const int d, MemoryPool& store)
{
    void* mem = store.allocate(sizeof(Node), alignof(Node));
    Node* root = new (mem) Node;
    if (d > 0) {
        root->l = make(d - 1, store);
        root->r = make(d - 1, store);
    } else {
        root->l = root->r = nullptr;
    }
    return root;
}

constexpr auto MIN_DEPTH = 4;

int main(int argc, char* argv[])
{
    const int max_depth = std::max(MIN_DEPTH + 2, (argc == 2 ? atoi(argv[1]) : 1
0));
    const int stretch_depth = max_depth + 1;

    // Alloc then dealloc stretchdepth tree.
    {
        MemoryPool store;

        Node* c = make(stretch_depth, store);
        std::cout << "stretch tree of depth " << stretch_depth << "\t "
                  << "check: " << c->check() << std::endl;
    }

    MemoryPool long_lived_store;
    Node* long_lived_tree = make(max_depth, long_lived_store);

    // Used as std::vector<std::pair<depth, checksum>>
    std::vector<std::pair<int, int>> results((max_depth - MIN_DEPTH) / 2 + 1);

    for (size_t i = 0; i < results.size(); ++i) {
        results[i].first = i * 2 + MIN_DEPTH;
    }

    std::for_each(std::execution::par,
        begin(results), end(results),
        [max_depth](auto& res) {
            int d = res.first;
            int iters = 1 << (max_depth - d + MIN_DEPTH);
            res.second = std::transform_reduce(std::execution::par,
                boost::counting_iterator<int>(0), boost::counting_iterator<int>(
iters),
                0,
                std::plus<> {},
                [d](int) {
                    thread_local std::pmr::unsynchronized_pool_resource upperPoo
l;
                    MemoryPool pool { &upperPool};
                    return make(d, pool)->check();
                });
        });

    for (const auto& [d, c] : results) {
        std::cout << (1 << (max_depth - d + MIN_DEPTH))
                  << "\t trees of depth " << d
                  << "\t check: " << c << "\n";
    }

    std::cout << "long lived tree of depth " << max_depth << "\t "
              << "check: " << (long_lived_tree->check()) << "\n";

    return 0;
}

