# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

# line-by-line from Greg Buchholz's C program



use strict;
use warnings;


my $bit_num = 0;
my $byte_acc = 0;
my ($i, $iter) = (0, 50);
my $limit = 2.0;
my $Zr, my $Zi, my $Cr, my $Ci, my $Tr, my $Ti;

my $w = shift;
my $h = $w;

print "P4\n$w $h\n";

for my $y (0..$h-1)
{
    for my $x (0..$w-1)
    {
        $Zr = 0.0; $Zi = 0.0;
        $Cr = 2.0*$x/$w - 1.5; $Ci = 2.0*$y/$h - 1.0;

        for (1..$iter)
        {
            $Tr = $Zr*$Zr - $Zi*$Zi + $Cr;
            $Ti = 2*$Zr*$Zi + $Ci;
            $Zr = $Tr; $Zi = $Ti;
            if ($Zr*$Zr+$Zi*$Zi > $limit*$limit) {
                last;
            }
        }
        if ($Zr*$Zr+$Zi*$Zi > $limit*$limit) {
            $byte_acc = ($byte_acc << 1) | 0x00;
        } else {
            $byte_acc = ($byte_acc << 1) | 0x01;
        }
        $bit_num = $bit_num + 1;

        if ($bit_num == 8)
        {
            print chr($byte_acc);
            $byte_acc = 0;
            $bit_num = 0;
        }
        elsif ($x == $w-1)
        {
            $byte_acc = $byte_acc << (8-$w%8);
            print chr($byte_acc);
            $byte_acc = 0;
            $bit_num = 0;
        }

   }
}


