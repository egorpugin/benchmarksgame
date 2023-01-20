/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Greg Buchholz
*/

#include<iostream>
#include<complex>

using namespace std;

int main (int argc, char **argv)
{
    int bit_num = 0;
    char byte_acc = 0;
    const int iter = 50;
    const double limit = 2.0;

    int w = atoi(argv[1]);
    int h = w;

    cout << "P4\n" << w << " " << h << endl;

    for(int y=0;y<h;y++)
    {
        for(int x=0;x<w;x++)
        {
            complex<double> Z(0.0,0.0);
            complex<double> C(2*(double)x/w - 1.5, 2*(double)y/h - 1);

            for (int i=0;i<iter;i++)
            {
                Z = Z*Z + C;
                if (norm(Z) > limit*limit)
                    break;
            }

            byte_acc = (byte_acc << 1) | ((norm(Z) > limit*limit) ? 0x00:0x01);
            bit_num++;

            if(bit_num == 8)
            {
                cout << byte_acc;
                byte_acc = 0;
                bit_num = 0;
            }
            else if(x == w-1)
            {
                byte_acc = byte_acc << (8-w%8);
                cout << byte_acc;
                byte_acc = 0;
                bit_num = 0;
            }
        }
    }

    return(0);
}

