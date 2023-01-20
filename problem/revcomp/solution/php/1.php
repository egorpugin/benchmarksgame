<?php
#
# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# reverse complement in PHP
# contributed by Danny Sauer
#

# Weʼll need some definitions
define( ʼLINE_LENGTHʼ, 60 );
define( ʼSRCʼ, ʼCGATMKRYVBHDʼ);
define( ʼDSTʼ, ʼGCTAKMYRBVDHʼ);
$str = ʼʼ;
$seq = ʼʼ;

# read in the file, a line at a time
while( !feof(STDIN) ) {
    $str = trim(fgets(STDIN));
    if( $str && $str[0] == ʼ>ʼ ){
        # if weʼre on a comment line, print the previous seq and move on
        print_seq();
        echo $str, "\n";
    }else{
        # otherwise, just append to the sequence
        $seq .= $str;
    }
}
print_seq();

exit;

# print the sequence out, if it exists
function print_seq(){
    global $seq; # no time-consuming argument passing for us! :)
    if($seq != ʼʼ){
        echo wordwrap( strrev( strtr(strtoupper($seq), SRC, DST) ),
                       LINE_LENGTH, "\n", true ), "\n";
    }
    $seq = ʼʼ;
}
?>

