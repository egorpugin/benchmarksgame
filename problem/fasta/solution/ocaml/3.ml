(* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Troestler Christophe
 * modified by Mauricio Fernandez
 * changed use of Strings to Bytes where mutation is needed, by Tony Tavener
 *)

(* Random number generator *)
let im = 139968
and ia = 3877
and ic = 29573

let last = ref 42 and im_f = float im
let gen_random  max =
  let n = (!last * ia + ic) mod im in
    last := n;
    max *. float n /. im_f

module Cumul_tbl =
struct
  type t = { probs : float array; chars : char array }

  let make a = let p = ref 0.0 in
    {
      probs = Array.map (fun (_, p1) -> p := !p +. p1; !p) a;
      chars = Array.map fst a;
    }

  let rand_char t =
    let p = gen_random 1.0 in
    let i = ref 0 and ps = t.probs in
      while p >= ps.(!i) do incr i done;
      t.chars.(!i)
end

let width = 60

let make_random_fasta id desc table n =
  Printf.printf ">%s %s\n" id desc;
  let table = Cumul_tbl.make table in
  let line = Bytes.make (width+1) ʼ\nʼ in
  for i = 1 to n / width do
    for j = 0 to width - 1 do Bytes.set line j (Cumul_tbl.rand_char table) done;
    print_bytes line;
  done;
  let w = n mod width in
  if w > 0 then (
    for j = 1 to w do print_char(Cumul_tbl.rand_char table); done;
    print_char ʼ\nʼ
  )

(* [write s i0 l w] outputs [w] chars of [s.[0 .. l]], followed by a
   newline, starting with [s.[i0]] and considering the substring [s.[0
   .. l]] as a "circle".
   One assumes [0 <= i0 <= l <= String.length s].
   @return [i0] needed for subsequent writes.  *)
let rec write s i0 l w =
  let len = l - i0 in
  if w <= len then (output_substring stdout s i0 w; print_char ʼ\nʼ; i0 + w)
  else (output_substring stdout s i0 len; write s 0 l (w - len))

let make_repeat_fasta id desc src n =
  Printf.printf ">%s %s\n" id desc;
  let l = String.length src
  and i0 = ref 0 in
  for i = 1 to n / width do
    i0 := write src !i0 l width;
  done;
  let w = n mod width in
  if w > 0 then ignore(write src !i0 l w)


let alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

let iub = [| (ʼaʼ, 0.27);  (ʼcʼ, 0.12);  (ʼgʼ, 0.12);  (ʼtʼ, 0.27);
             (ʼBʼ, 0.02);  (ʼDʼ, 0.02);  (ʼHʼ, 0.02);  (ʼKʼ, 0.02);
             (ʼMʼ, 0.02);  (ʼNʼ, 0.02);  (ʼRʼ, 0.02);  (ʼSʼ, 0.02);
             (ʼVʼ, 0.02);  (ʼWʼ, 0.02);  (ʼYʼ, 0.02);  |]

let homosapiens = [| (ʼaʼ, 0.3029549426680);    (ʼcʼ, 0.1979883004921);
                     (ʼgʼ, 0.1975473066391);    (ʼtʼ, 0.3015094502008);  |]

let () =
  let n = try int_of_string(Array.get Sys.argv 1) with _ -> 1000 in
  make_repeat_fasta "ONE" "Homo sapiens alu" alu (n*2);
  make_random_fasta "TWO" "IUB ambiguity codes" iub (n*3);
  make_random_fasta "THREE" "Homo sapiens frequency" homosapiens (n*5)


