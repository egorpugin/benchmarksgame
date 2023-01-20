/* The Computer Language Benchmarks Game
https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

contributed by Seth Heeren
*/
#include <iostream>
#include <string>

using std::string;
using std::cin;
using std::cout;
using std::getline;
using std::endl;

template <typename Ch> inline Ch complement(Ch c)
{
    switch (c)
    {
        // IDEA: Reorder branches after profiling?
        // IDEA: (gcc probably compiles the switch into a jump table)
        case ʼtʼ: case ʼTʼ:             //  7707842
        case ʼuʼ: case ʼUʼ: return ʼAʼ; //
        case ʼaʼ: case ʼAʼ: return ʼTʼ; //  7592592
        case ʼgʼ: case ʼGʼ: return ʼCʼ; //  5552804
        case ʼcʼ: case ʼCʼ: return ʼGʼ; //  5442702
        case ʼvʼ: case ʼVʼ: return ʼBʼ; //   205714
        case ʼsʼ: case ʼSʼ: return ʼSʼ; //   200078
        case ʼhʼ: case ʼHʼ: return ʼDʼ; //   197260
        case ʼwʼ: case ʼWʼ: return ʼWʼ; //   194442
        case ʼrʼ: case ʼRʼ: return ʼYʼ; //   194442
        case ʼmʼ: case ʼMʼ: return ʼKʼ; //   174716
        case ʼyʼ: case ʼYʼ: return ʼRʼ; //   157808
        case ʼkʼ: case ʼKʼ: return ʼMʼ; //   154990
        case ʼbʼ: case ʼBʼ: return ʼVʼ; //   146536
        case ʼdʼ: case ʼDʼ: return ʼHʼ; //   132446
        case ʼnʼ: case ʼNʼ: return ʼNʼ; //   129628
    }
    throw "parse error"; // TODO proper exception
}

template <typename Out>
inline static void print_reverse(std::string const& sequence, Out& out)
{
    auto const rend = sequence.rend();
    size_t count = 0;
    for (auto i = sequence.rbegin(); i != rend; ++i)
    {
        *out++ = *i; // TODO: buffer writes and append line by line?
        if (0 == ++count % 60)
            *out++ = ʼ\nʼ;
    }
    if (count % 60)
        *out++ = ʼ\nʼ;
}

int main()
{
    string sequence, line;
    sequence.reserve(12000); // arbitrary heuristic preallocation

    cin.unsetf(std::ios::skipws);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    auto out = std::ostreambuf_iterator<char>(std::cout);
    while (getline(cin, line))
    {
        const bool is_header = (line[0] == ʼ>ʼ);

        if (is_header)
        {
            if (!sequence.empty())
            {
                for (auto& c : sequence)
                    c = complement(c);
                print_reverse(sequence, out);
            }
            // clear, (retain allocated capacity)
            sequence.resize(0);

            // print header line
            cout << line << endl;
        }
        else
        {
            sequence.append(line);
        }
    }

    if (!sequence.empty())
    {
        for (auto& c : sequence)
            c = complement(c);
        print_reverse(sequence, out);
    }
}

