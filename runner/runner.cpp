// SPDX-License-Identifier: AGPL-3.0-only
// Copyright (C) 2022 Egor Pugin <egor.pugin@gmail.com>

#include "command.h"

using namespace sw;

int main() {
    path problems = "problem";
    const auto data_generator = "data_generator.cpp";
    for (path problem : fs::directory_iterator{problems}) {
        if (fs::exists(problem / data_generator)) {

        }
        std::cerr << problem << "\n";
    }
}
