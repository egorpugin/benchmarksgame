void build(solution &s) {
    static resource_pool pool = 1;
    path fasta_output;
    for (auto &&dir : {"fasta"s, "reverse-complement"s}) {
        auto fulldir = s.source_dir / "problem" / dir / "solution";
        for (path fn : fs::directory_iterator{fulldir}) {
            auto &t = s.add<executable>(dir + "."s + fn.stem().string());
            t += fn;

            auto c = t.add_test();
            c.set_resource_pool(pool);

            // data for revcomp
            if (dir == "fasta"s) {
                c += 100000;
                if (fn.filename() == "solution2.cpp") {
                    fasta_output = std::get<path>(c.out);
                }
            } else if (dir == "reverse-complement"s) {
                c < fasta_output;
            }
        }
    }
}
