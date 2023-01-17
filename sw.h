// solutions can be taken from
// https://benchmarksgame-team.pages.debian.net/benchmarksgame/fastest/cpp.html
// & other langs

void build(solution &s) {
    static resource_pool pool = 1;
    path fasta_output;
    for (auto &&dir : {"fasta"s, "reverse-complement"s}) {
        auto fulldir = s.source_dir / "problem" / dir / "solution";
        for (path fn : fs::recursive_directory_iterator{fulldir}) {
            auto namep = fn.lexically_relative(fulldir);
            namep = namep.parent_path() / namep.stem();
            auto name = namep.string();
            std::replace(name.begin(), name.end(), '\\', '.');
            std::replace(name.begin(), name.end(), '/', '.');
            auto &t = s.add<executable>(dir + "."s + name);
            t += fn;

            auto c = t.add_test();
            c.set_resource_pool(pool);

            if (dir == "fasta"s) {
                c += 100000;
                if (fn.filename() == "solution2.cpp") {
                    // data for revcomp etc.
                    fasta_output = std::get<path>(c.out);
                }
            } else if (dir == "reverse-complement"s) {
                c < fasta_output;
            }
        }
    }
}
