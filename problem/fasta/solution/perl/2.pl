# The Computer Language Benchmarks game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by David Pyke
# tweaked by Danny Sauer

use constant IM => 139968;
use constant IA => 3877;
use constant IC => 29573;

use constant LINELENGTH => 60;

my $LAST = 42;
sub gen_random ($) {
    return ( ($_[0] * ($LAST = ($LAST * IA + IC) % IM)) / IM );
}

sub makeCumulative($){
    my($genelist) = @_;
    $cp = 0.0;

    foreach (@$genelist){
        $_->[1] = $cp += $_->[1];
    }
}

sub selectRandom($){
    my($genelist) = @_;
    $r = gen_random (1);

    foreach (@$genelist){
        if ($r < $_->[1]){ return $_->[0]; }
    }
}


sub makeRandomFasta($$$$){
#void makeRandomFasta (const char * id, const char * desc, const struct aminoaci
ds * genelist, int count, int n) {
    my($id,$desc,$n,$genelist) = @_;

    print ">$id $desc\n";
    $pick=ʼʼ;

    # print whole lines
    foreach (1 .. int($n / LINELENGTH) ){
        foreach (1 ..  LINELENGTH ){
            $pick .= selectRandom($genelist);
        }
        print "$pick\n";
        $pick = ʼʼ;
    }
    #print remaining line (if required)
    if ($n % LINELENGTH){
        foreach (1 ..  $n % LINELENGTH ){
            $pick .= selectRandom($genelist);
        }
        print "$pick\n";
    }
}

sub makeRepeatFasta($$$$){
#void makeRepeatFasta (const char * id, const char * desc, const char * s, int n
) {
    # we want to print $n characters of $s (repeated if nessary) with newlines e
very LINELENGTH
    my($id,$desc,$s,$n) = @_;

    print ">$id $desc\n";

    # what we need, and the extra (if any) will be discarded.
    foreach (1 .. int($n / LINELENGTH) ){
        while (length $ss < LINELENGTH){
            $ss .= $s;
        }
        print substr($ss,0,LINELENGTH), "\n";
        $ss = substr($ss,LINELENGTH);
    }
    #final_line
    while (length $ss < LINELENGTH){
        $ss .= $s;
    }
    print substr($ss, 0, ($n % LINELENGTH)), "\n";
print STDERR "\n";
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

$alu =
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

makeCumulative $iub;
makeCumulative $homosapiens;

makeRepeatFasta (ʼONEʼ, ʼHomo sapiens aluʼ, $alu, $n*2);
makeRandomFasta (ʼTWOʼ, ʼIUB ambiguity codesʼ, $n*3, $iub);
makeRandomFasta (ʼTHREEʼ, ʼHomo sapiens frequencyʼ, $n*5, $homosapiens);

exit 0;

#END OF FILE

