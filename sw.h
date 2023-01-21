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

    // deps:
    // boost, openmp, pcre, apr
    for (auto &&dir : {
        "binarytrees"s,
        "fannkuchredux"s,
        "fasta"s,
        //"knucleotide"s,
        "mandelbrot"s,
        "nbody"s,
        "pidigits"s,
        "regexredux"s,
        "revcomp"s,
        "simple"s,
        "spectralnorm"s,
        }) {
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
            if (!t.is<os::windows>()) {
                t += "-march=native"_copt;
                t += "-fomit-frame-pointer"_copt;
            }
            t += "/usr/include/apr-1"_idir;

            auto c = t.add_test(dir + "."s + name);
            c.set_resource_pool(pool);

            if (0) {
            } else if (dir == "binarytrees"s) {
                c += 21;
            } else if (dir == "fannkuchredux"s) {
                c += 12;
            } else if (dir == "fasta"s) {
                c += fasta_size;
            } else if (dir == "knucleotide"s) {
            } else if (dir == "mandelbrot"s) {
                c += 16000;
            } else if (dir == "nbody"s) {
                c += 50000000;
            } else if (dir == "pidigits"s) {
                c += 10000;
            } else if (dir == "regexredux"s) {
                if (!t.is<os::windows>()) {
                    t += "-lpcre"_lopt;
                    t += "-lpcre2-8"_lopt;
                }
            } else if (dir == "revcomp"s) {
                c < fasta_output;
            } else if (dir == "simple"s) { // these are simple mandelbrot
                c += 16000;
            } else if (dir == "spectralnorm"s) {
                if (!t.is<os::windows>()) {
                    t += "-fopenmp"_lopt;
                }
                c += 5500;
            }
        }
    }
}
