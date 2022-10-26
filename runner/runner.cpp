// SPDX-License-Identifier: AGPL-3.0-only
// Copyright (C) 2022 Egor Pugin <egor.pugin@gmail.com>

#include "sw/command.h"
#include "sw/solution.h"

using namespace sw;

struct revcomp {
    void prepare() {
    }
};

using problem = variant<revcomp>;

void main1() {
    path problems = "problem";
    const auto data_generator = "data_generator";
    for (path problem : fs::directory_iterator{problems}) {
        solution s;
        auto dgen = problem / data_generator += ".cpp";
        auto test_data = s.binary_dir / problem / "test.out";
        if (fs::exists(dgen) && !fs::exists(test_data)) {
            executor ex;

            auto &t = s.add<executable_target>(data_generator);
            t += dgen;
            s.build(ex);

            raw_command c;
            c += t.executable, 1000;
            c > test_data;
            fs::create_directories(test_data.parent_path());
            c.run(ex);
            ex.run();
        }
        std::cerr << problem << "\n";
    }
}

int main() {
    try {
        main1();
        return 0;
    } catch (std::exception &e) {
        std::cerr << e.what();
    } catch (...) {
        std::cerr << "unknown exception";
    }
    return 1;
}
