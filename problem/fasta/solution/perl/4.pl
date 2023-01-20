# The Computer Language Benchmarks game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by David Pyke
# tweaked by Danny Sauer
# optimized by Steffen Mueller
# tweaked by Kuang-che Wu

use strict;
use warnings;
use constant IM => 139968;
use constant IA => 3877;
use constant IC => 29573;

use constant LINELENGTH => 60;

my $LAST = 42;
sub gen_random {
    return map {( ($_[0] * ($LAST = ($LAST * IA + IC) % IM)) / IM )} 1..($_[1]||
1);
}

sub makeCumulative {
    my $genelist = shift;
    my $cp = 0.0;

    $_->[1] = $cp += $_->[1] foreach @$genelist;
}

sub selectRandom {
    my $genelist = shift;
    my $number = shift || 1;
    my @r = gen_random(1, $number);

    my $s;
    foreach my $r (@r) {
        foreach (@$genelist){
            if ($r < $_->[1]) { $s .= $_->[0]; last; }
        }
    }

    return $s;
}


sub makeRandomFasta {
    my ($id, $desc, $n, $genelist) = @_;

    print ">", $id, " ", $desc, "\n";

    # print whole lines
    foreach (1 .. int($n / LINELENGTH) ){
        print selectRandom($genelist, LINELENGTH), "\n";
    }
    # print remaining line (if required)
    if ($n % LINELENGTH){
        print selectRandom($genelist, $n % LINELENGTH), "\n";
    }
}

sub makeRepeatFasta {
    my ($id, $desc, $s, $n) = @_;

    print ">", $id, " ", $desc, "\n";

    my $r = length $s;
    my $ss = $s . $s . substr($s, 0, $n % $r);
    for my $j(0..int($n / LINELENGTH)-1) {
        my $i = $j*LINELENGTH % $r;
        print substr($ss, $i, LINELENGTH), "\n";
    }
    if ($n % LINELENGTH) {
        print substr($ss, -($n % LINELENGTH)), "\n";
    }
}


my $iub = [
    [ ʼaʼ, 0.27 ],
    [ ʼcʼ, 0.12 ],
    [ ʼgʼ, 0.12 ],
    [ ʼtʼ, 0.27 ],
    [ ʼBʼ, 0.02 ],
    [ ʼDʼ, 0.02 ],
    [ ʼHʼ, 0.02 ],
    [ ʼKʼ, 0.02 ],
    [ ʼMʼ, 0.02 ],
    [ ʼNʼ, 0.02 ],
    [ ʼRʼ, 0.02 ],
    [ ʼSʼ, 0.02 ],
    [ ʼVʼ, 0.02 ],
    [ ʼWʼ, 0.02 ],
    [ ʼYʼ, 0.02 ]
];

my $homosapiens = [
    [ ʼaʼ, 0.3029549426680 ],
    [ ʼcʼ, 0.1979883004921 ],
    [ ʼgʼ, 0.1975473066391 ],
    [ ʼtʼ, 0.3015094502008 ]
];

my $alu =
    ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGʼ .
    ʼGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGAʼ .
    ʼCCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATʼ .
    ʼACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAʼ .
    ʼGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGʼ .
    ʼAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCʼ .
    ʼAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ;

######################################################################
#main

my $n = ($ARGV[0] || 1000) ;

makeCumulative($iub);
makeCumulative($homosapiens);

makeRepeatFasta (ʼONEʼ, ʼHomo sapiens aluʼ, $alu, $n*2);
makeRandomFasta (ʼTWOʼ, ʼIUB ambiguity codesʼ, $n*3, $iub);
makeRandomFasta (ʼTHREEʼ, ʼHomo sapiens frequencyʼ, $n*5, $homosapiens);


