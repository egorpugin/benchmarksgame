void build(solution &s) {
    auto &fasta = s.add<executable>("fasta");
    fasta += "problem/fasta/solution/solution1.c";

    auto &revcomp = s.add<executable>("revcomp");
    revcomp += "problem/revcomp/solution/solution1.c";
}
