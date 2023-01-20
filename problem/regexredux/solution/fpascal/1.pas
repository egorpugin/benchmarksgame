Program regexredux;

(*
  The Computer Language Benchmarks Game
  https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

  contributed by Vitaly Trifonov
  adapted for ʼreduxʼ by Peter Blackman
*)

{$mode objfpc}
uses sysutils;


(******************************    pcre wrap   *****************************)

const
  libpcre = ʼpcreʼ;
  PCRE_STUDY_JIT_COMPILE = $00001;


type
  pcre = Pointer;
  pcre_extra = Pointer;
  PPChar = ^PChar;


function pcre_compile( const pattern: PChar;
                       options: Integer;
                       const errptr: PPChar;
                       erroffset: PInteger;
                       const tableptr: PChar ): pcre; cdecl; external libpcre;

function pcre_exec( const code: pcre;
                    const extra: pcre_extra;
                    const subject: PChar;
                    length, startoffset, options: Integer;
                    ovector: PInteger;
                    ovecsize: Integer ): Integer; cdecl; external libpcre;

function pcre_study( const external_re: pcre;
                     options: integer;
                     errorptr: PPChar ): pcre_extra; cdecl; external libpcre;

(***************************************************************************)

const
    patt: array[1..5] of pChar = (
        ʼtHa[Nt]ʼ,
        ʼaND|caN|Ha[DS]|WaSʼ,
        ʼa[NSt]|BYʼ,
        ʼ<[^>]*>ʼ,
        ʼ\|[^|][^|]*\|ʼ);

    repl: array[1..5] of pChar = (ʼ<4>ʼ, ʼ<3>ʼ, ʼ<2>ʼ, ʼ|ʼ, ʼ-ʼ);


var
  patterns: array[1..9] of PChar =
    (
      ʼagggtaaa|tttaccctʼ,
      ʼ[cgt]gggtaaa|tttaccc[acg]ʼ,
      ʼa[act]ggtaaa|tttacc[agt]tʼ,
      ʼag[act]gtaaa|tttac[agt]ctʼ,
      ʼagg[act]taaa|ttta[agt]cctʼ,
      ʼaggg[acg]aaa|ttt[cgt]ccctʼ,
      ʼagggt[cgt]aa|tt[acg]accctʼ,
      ʼagggta[cgt]a|t[acg]taccctʼ,
      ʼagggtaa[cgt]|[acg]ttaccctʼ
    );


(* Count match with pattern of regexp in seq buffer. *)
function count( const pattern, seq: PChar; len: Integer ): Longint;
var
  cre: pcre;
  cre_ex: pcre_extra;
  err: PChar;
  ofs: Integer;
  ind: Longint = 0;
  m: array[0..2] of Integer;
begin
  cre := pcre_compile(pattern, 0, @err, @ofs, nil);
  cre_ex := pcre_study(cre, PCRE_STUDY_JIT_COMPILE, @err);
  m[1] := 0;

  while pcre_exec(cre,   cre_ex, seq, len,   m[1], 0, m, 3) >= 0 do
    ind += 1;

  count := ind
end;

function split_count ( const pattern, seq: PChar; len: Integer ): Longint;
var
  split: PChar;
  vcount: Longint;
begin
  split := strscan(pattern, ʼ|ʼ);
  Byte(split^) := 0;

  vcount := count(pattern, seq, len);
  vcount += count(@split[1], seq, len);

  split^ := ʼ|ʼ;
  split_count := vcount
end;

(* Substitute pattern of regexp with repl, return new length. *)
function subst( const pattern, repl: PChar; var seq: PChar; len: Integer ): Long
int;
var
  cre: pcre;
  cre_ex: pcre_extra;
  err: PChar;
  ofs: Integer;
  size_repl, size, bsize, pos: Longint;
  m: array[0..2] of Integer;
  newSeq, otmpseq: PChar;
begin
  cre := pcre_compile(pattern, 0, @err, @ofs, nil);
  cre_ex := pcre_study(cre, PCRE_STUDY_JIT_COMPILE, @err);
  size_repl := strlen(repl);
  m[1] := 0; size := 0;

(* Calculate required size for malloc. *)
  while pcre_exec(cre,   cre_ex, seq, len,   m[1], 0, m, 3) >= 0 do
    size += size_repl - m[1] + m[0];
  size += len;

  GetMem(newSeq, SizeOf(Char)*size);

(* Do substitute. *)
  m[1] := 0; pos := 0;
  otmpseq := newSeq;


  if size_repl <> 0 then
    while pcre_exec(cre,   cre_ex, seq, len,   m[1], 0, m, 3) >= 0 do
    begin
      bsize := m[0] - pos;
      strlcopy(otmpseq, @seq[pos], bsize);

      otmpseq := @otmpseq[bsize];
      pos := m[1];

      otmpseq := strecopy(otmpseq, repl);
    end
  else
    while pcre_exec(cre,   cre_ex, seq, len,   m[1], 0, m, 3) >= 0 do
    begin
      bsize := m[0] - pos;
      strlcopy(otmpseq, @seq[pos], bsize);

      otmpseq := @otmpseq[bsize];
      pos := m[1];
    end;

  strcopy(otmpseq, @seq[pos]);

  FreeMem(seq);
  seq := newSeq;

  subst := size
end;


var
  i, seqLen : Longint;
  readLen   : Longint = 0;
  maxSeqLen : Longint = 6000000;
  seq       : PChar;

begin
  GetMem(seq, SizeOf(Char)*(maxSeqLen+1));

(* Read FASTA format file from stdin and count length. *)
  while not eof do
  begin
    if readLen = maxSeqLen then
    begin
      maxSeqLen += 3000000;
      seq := ReAllocMem(seq, SizeOf(Char)*(maxSeqLen+1));
    end;
    read(seq[readLen]);
    readLen += 1
  end;
  Byte(seq[readLen]) := 0; //end read data


(* Remove FASTA sequence descriptions and all linefeed characters.  *)
  seqLen := subst(ʼ>.*|\nʼ, ʼʼ, seq, readLen);


(* Count all matches of patterns[i] in  seq buffer. *)
  for i := 1 to length (patterns) do
    writeln(patterns[i], ʼ ʼ, split_count(patterns[i], seq, seqLen));

  writeln;
  writeln(readLen);
  writeln(seqLen);

(* All IUB substitutes. *)
  for i := 1 to length(patt) do
    seqLen := subst(patt[i], repl[i], seq, seqLen);

  writeln(seqLen);

  FreeMem(seq);
end.

