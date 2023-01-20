// solutions can be taken from
// https://benchmarksgame-team.pages.debian.net/benchmarksgame/fastest/cpp.html
// & other langs

void build(solution &s) {
    static resource_pool pool = 1;

    int fasta_size = 100000;
    int revcomp_size = fasta_size;
    path fasta_output;
    {
        auto &t = s.add<executable>("data_generator");
        t += "problem/revcomp/data_generator.cpp";
        auto c = t.add_command();
        c += revcomp_size;
        c > t.binary_dir / "data.txt";
        fasta_output = c.out;
    }

    for (auto &&dir : {"fasta"s, "revcomp"s}) {
        auto fulldir = s.source_dir / "problem" / dir / "solution";
        for (path fn : fs::recursive_directory_iterator{fulldir}) {
            if (!fs::is_regular_file(fn) || fn.filename().string().starts_with(".")) {
                continue;
            }
            if (!(fn.extension() == ".c" || fn.extension() == ".cpp")) {
                continue;
            }
            auto namep = fn.lexically_relative(fulldir);
            namep = namep.parent_path() / namep.stem();
            auto name = namep.string();
            std::replace(name.begin(), name.end(), '\\', '.');
            std::replace(name.begin(), name.end(), '/', '.');
            auto &t = s.add<executable>(dir + "."s + name);
            t += fn;

            auto c = t.add_test(dir + "."s + name);
            c.set_resource_pool(pool);

            if (0) {
            } else if (dir == "fasta"s) {
                c += fasta_size;
            } else if (dir == "revcomp"s) {
                c < fasta_output;
            }
        }
    }
}
