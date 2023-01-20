# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
#   contributed by Robert Bradshaw
#      modified by Ruud H.G.van Tol
#      modified by Emanuele Zeppieri

use strict;

use Math::BigInt lib => ʼGMPʼ;

die ʼMath::BigInt::GMP missing!ʼ
    if Math::BigInt->config->{lib} ne ʼMath::BigInt::GMPʼ;

my $z0 = Math::BigInt->new(1);
my $z1 = Math::BigInt->new(0);
my $z2 = Math::BigInt->new(1);

sub extract_digit { return scalar( ($z0 * $_[0] + $z1) / $z2 ) }

sub compose {
    if ( defined $_[3] ) {
        $z1->bmul( $_[0] )->badd( $_[1] * $z2 )
    } else {
        $z1->bmul( $_[2] )->badd( $_[1] * $z0 )
    }
    $z0->bmul( $_[0] );
    $z2->bmul( $_[2] );
    return
}

my $n = $ARGV[0];
($,, $\) = ("\t", "\n");
my ($i, $s, $d); my $k = 0;

# main loop
for $i (1..$n) {
    while (
        $z0->bcmp($z2) == 1 || ( $d = extract_digit(3) ) != extract_digit(4)
    ) {
        # y not safe
        $k++; compose($k, 4*$k+2, 2*$k+1)
    }
    compose(10, -10*$d, 1, 1);
    $s .= $d;

    unless ( $i % 10 ) { print $s, ":$i"; undef $s }
}

$s .= ʼ ʼ x (10-$i) if $i = $n % 10;

print $s, ":$n" if $s

