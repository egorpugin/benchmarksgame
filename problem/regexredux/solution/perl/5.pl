# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Eelko de Vos, 2021
#
# This version splits up the first match counts into forked processes,
# then continues to do the "hard" search-replaces and when that ends
# starts to look for the answers of the childs to the first matches.
#
# Itʼs all file-based, no pipes used here. That could be a small
# optimization but it wonʼt make the program significantly faster.


use strict;

# Hold my answers, please
my @length;

# Take in the entire file
my $file_in=do { local $/; <STDIN> };
$length[0]=length($file_in)-1;


$file_in=~s/\>.*?\n|\n//gs;
$length[1]=length($file_in);


my @match_these = (
    ʼagggtaaa|tttaccctʼ,
    ʼ[cgt]gggtaaa|tttaccc[acg]ʼ,
    ʼa[act]ggtaaa|tttacc[agt]tʼ,
    ʼag[act]gtaaa|tttac[agt]ctʼ,
    ʼagg[act]taaa|ttta[agt]cctʼ,
    ʼaggg[acg]aaa|ttt[cgt]ccctʼ,
    ʼagggt[cgt]aa|tt[acg]accctʼ,
    ʼagggta[cgt]a|t[acg]taccctʼ,
    ʼagggtaa[cgt]|[acg]ttaccctʼ);



# create all answers, in forked children
my $cntr=0;
foreach my $search (@match_these) {
    # remove all counter files if they are still present
    unlink "$cntr.dat";

    if (!fork()) {
        my @matches=($file_in=~/$search/gs);
        my $count=scalar(@matches);

        open W,">$cntr.dat";
        print W "$search\t$count\n";
        close W;

        exit(0);
    }
    $cntr++;
}

# meanwhile, start processing the last answers
my %search_replace = (
    ʼtHa[Nt]ʼ => ʼ<4>ʼ,
    ʼaND|caN|Ha[DS]|WaSʼ => ʼ<3>ʼ,
    ʼa[NSt]|BYʼ => ʼ<2>ʼ,
    ʼ<[^>]*>ʼ => ʼ|ʼ,
    ʼ\\|[^\|][^\|]*\\|ʼ => ʼ-ʼ );

# We need this exact order so we need to explicitely again set these
# keys here:
my @search_replace = (
    ʼtHa[Nt]ʼ,
    ʼaND|caN|Ha[DS]|WaSʼ,
    ʼa[NSt]|BYʼ,
    ʼ<[^>]*>ʼ,
    ʼ\\|[^\|][^\|]*\\|ʼ);

# Now do the costly search-replaces
foreach my $key (@search_replace) {
    $file_in=~s/$key/$search_replace{$key}/gs;
}

# All done!
# Now wait for and print results of the childs using a busy wait
for (my $cntr=0; $cntr<scalar(@match_these); $cntr++) {
    while (!-f "$cntr.dat") {
    }
    my $str="";
    while ($str eq "") {
        open F,"<$cntr.dat";
        $str = join("",<F>);
        close F;
    }
    print $str;
}
$length[2]=length($file_in);


# print the results...
print "\n".$length[0]."\n";
print $length[1]."\n";
print $length[2]."\n";

