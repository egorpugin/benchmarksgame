void build(solution &s) {
    auto &fasta = s.add<executable>("fasta");
    //fasta += "problem/fasta/solution/solution1.c";
    fasta += "problem/fasta/solution/solution2.cpp";

    auto &revcomp = s.add<executable>("revcomp");
    //revcomp += "problem/reverse-complement/solution/solution1.c";
    revcomp += "problem/reverse-complement/solution/solution2.cpp";

    //auto &t = s.add<test>();
    //t += fasta, 100000;

    static resource_pool pool = 1;

    path p;
    auto n = 10000000;
    // auto n = 100000;
    auto f = fasta.add_test("test");
    {
        f += n;
        f.set_resource_pool(pool);
    }
    {
        auto c = revcomp.add_test("test");
        c < std::get<path>(f.out);
        c.set_resource_pool(pool);
    }
}
