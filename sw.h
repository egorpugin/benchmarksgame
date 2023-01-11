void build(solution &s) {
    auto &fasta = s.add<executable>("fasta");
    //fasta += "problem/fasta/solution/solution1.c";
    fasta += "problem/fasta/solution/solution2.cpp";

    auto &revcomp = s.add<executable>("revcomp");
    //revcomp += "problem/reverse-complement/solution/solution1.c";
    revcomp += "problem/reverse-complement/solution/solution2.cpp";

    //auto &t = s.add<test>();
    //t += fasta, 100000;


    path p;
    auto n = 10000000;
    {
        auto c = fasta.add_test("test");
        c += n;
        p = c > "out.txt";
    }
    {
        auto c = revcomp.add_test("abc");
        c < p;
        c > "out.txt";
    }

    {
        auto c = fasta.add_test();
        c += n;
        //auto c2 = revcomp.add_test();
        //c | c2;
    }

    {
        auto c = fasta.add_test();
        c += n;
        /*auto c2 = revcomp.add_test();
        c | c2;
        auto c3 = revcomp.add_test();
        c | c3;*/
    }
}
