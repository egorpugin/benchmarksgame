/* ------------------------------------------------------------------ */
/* The Computer Language Benchmarks Game                              */
/* https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
                */
/*                                                                    */
/* Contributed by Anthony Borla                                       */
/* Modified by Vaclav Haisman                                         */
/* Changed to match style of Perl example: Greg Buchholz              */
/* ------------------------------------------------------------------ */

#include <cctype>
#include <string>
#include <algorithm>
#include <iterator>
#include <iostream>
using namespace std;

const int LINELENGTH = 60;

typedef string Header;
typedef string Segment;

inline char complement(char element)
{
  static const char charMap[] =
    {
      ʼTʼ, ʼVʼ, ʼGʼ, ʼHʼ, ʼ\0ʼ, ʼ\0ʼ, ʼCʼ, ʼDʼ, ʼ\0ʼ, ʼ\0ʼ, ʼMʼ, ʼ\0ʼ, ʼKʼ,
      ʼNʼ, ʼ\0ʼ, ʼ\0ʼ, ʼ\0ʼ, ʼYʼ, ʼSʼ, ʼAʼ, ʼAʼ, ʼBʼ, ʼWʼ, ʼ\0ʼ, ʼRʼ, ʼ\0ʼ
    };

  return charMap[toupper(element) - ʼAʼ];
}

void print_revcomp(Header const& header, Segment const& seg, ostream& out = std:
:cout)
{
    out << header << "\n";

    Segment comp(seg.rbegin(),seg.rend());
    transform(comp.begin(),comp.end(), comp.begin(), complement);

    size_t i = 0;
    size_t stop = comp.length()/LINELENGTH + ((comp.length()%LINELENGTH)?1:0);

    while(i < stop)
        out << comp.substr(i++*LINELENGTH,LINELENGTH) << "\n";
}

int main ()
{
  ios_base::sync_with_stdio(false);

  Segment line, segment;
  Header header;

  while (getline(cin, line))
  {
      if (line[0] == ʼ>ʼ)
      {
          if (! segment.empty())
            print_revcomp(header, segment);
          header = line;
          segment.clear();
      }
      else
          segment += line;
  }
  print_revcomp(header, segment);

  return 0;
}


