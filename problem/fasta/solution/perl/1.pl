# The Computer Language Benchmarks game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Barry Walsh

# port of fasta.rb #6

use strict;
use warnings;
use feature ʼsayʼ;

use constant IM => 139968;
use constant IA => 3877;
use constant IC => 29573;

my $LAST = 42;

my $alu =
    ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGʼ .
    ʼGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGAʼ .
    ʼCCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATʼ .
    ʼACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAʼ .
    ʼGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGʼ .
    ʼAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCʼ .
    ʼAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ;

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

sub make_repeat_fasta {
    my ($src, $n) = @_;
    my $width = qr/(.{1,60})/;
    my $l     = length $src;
    my $s     = $src x (($n / $l) + 1);
    substr($s, $n, $l) = ʼʼ;

    while ($s =~ m/$width/g) { say $1 }
#   say for unpack ʼ(a60)*ʼ, $s;   # slower than above over larger strings
}

sub make_random_fasta {
    my ($table, $n) = @_;
    my $rand   = undef;
    my $width  = 60;
    my $prob   = 0.0;

    $_->[1] = ($prob += $_->[1]) for @$table;

    my $collector = ʼ$rand = ($LAST = ($LAST * IA + IC) % IM) / IM;ʼ;
    $collector .= "print(ʼ$_->[0]ʼ) && next if $_->[1] > \$rand;\n" for @$table;

    my $code = q{
        for (1..($n / $width)) {
            for (1..$width) { !C! }
            print "\n";
        }
        if ($n % $width != 0) {
            for (1 .. $n % $width) { !C! }
            print "\n";
        }
    };
    $code =~ s/!C!/$collector/g;
    eval $code;
}



my $n = $ARGV[0] || 27;

say ">ONE Homo sapiens alu";
make_repeat_fasta($alu, $n*2);

say ">TWO IUB ambiguity codes";
make_random_fasta($iub, $n*3);

say ">THREE Homo sapiens frequency";
make_random_fasta($homosapiens, $n*5);


