(* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * Contributed by Paolo Ribeca
 *)

let chars_per_line = 60
and lines_per_worker =
  match Sys.word_size with
  | 32 -> 200000
  | 64 -> 500000
  | _ -> assert false

let rc_table = String.make 256 ʼ\000ʼ
let _ =
  rc_table.[Char.code ʼAʼ] <- ʼTʼ; rc_table.[Char.code ʼTʼ] <- ʼAʼ;
  rc_table.[Char.code ʼwʼ] <- ʼWʼ; rc_table.[Char.code ʼsʼ] <- ʼSʼ;
  rc_table.[Char.code ʼaʼ] <- ʼTʼ; rc_table.[Char.code ʼtʼ] <- ʼAʼ;
  rc_table.[Char.code ʼCʼ] <- ʼGʼ; rc_table.[Char.code ʼGʼ] <- ʼCʼ;
  rc_table.[Char.code ʼcʼ] <- ʼGʼ; rc_table.[Char.code ʼgʼ] <- ʼCʼ;
  rc_table.[Char.code ʼUʼ] <- ʼAʼ; rc_table.[Char.code ʼuʼ] <- ʼAʼ;
  rc_table.[Char.code ʼMʼ] <- ʼKʼ; rc_table.[Char.code ʼKʼ] <- ʼMʼ;
  rc_table.[Char.code ʼmʼ] <- ʼKʼ; rc_table.[Char.code ʼkʼ] <- ʼMʼ;
  rc_table.[Char.code ʼRʼ] <- ʼYʼ; rc_table.[Char.code ʼYʼ] <- ʼRʼ;
  rc_table.[Char.code ʼrʼ] <- ʼYʼ; rc_table.[Char.code ʼyʼ] <- ʼRʼ;
  rc_table.[Char.code ʼWʼ] <- ʼWʼ; rc_table.[Char.code ʼSʼ] <- ʼSʼ;
  rc_table.[Char.code ʼwʼ] <- ʼWʼ; rc_table.[Char.code ʼsʼ] <- ʼSʼ;
  rc_table.[Char.code ʼVʼ] <- ʼBʼ; rc_table.[Char.code ʼBʼ] <- ʼVʼ;
  rc_table.[Char.code ʼvʼ] <- ʼBʼ; rc_table.[Char.code ʼbʼ] <- ʼVʼ;
  rc_table.[Char.code ʼHʼ] <- ʼDʼ; rc_table.[Char.code ʼDʼ] <- ʼHʼ;
  rc_table.[Char.code ʼhʼ] <- ʼDʼ; rc_table.[Char.code ʼdʼ] <- ʼHʼ;
  rc_table.[Char.code ʼNʼ] <- ʼNʼ; rc_table.[Char.code ʼnʼ] <- ʼNʼ

let _ =
  let aug_chars_per_line = chars_per_line + 1
  and in_ack, out_ack = Unix.pipe () and in_end, out_end = Unix.pipe ()
  and put out_pipe () =
    if Unix.write out_pipe " " 0 1 <> 1 then
      failwith "Pipe problem"
  and get in_pipe () =
    let res = " " in
    if Unix.read in_pipe res 0 1 <> 1 then
      failwith "Pipe problem" in
  let put_ack = put out_ack and get_ack = get in_ack
  and put_end_ack = put out_end and get_end_ack = get in_end in
  let rec spawn tag beg first =
    let output_tag () =
      print_string tag;
      print_char ʼ\nʼ;
      flush stdout
    and buf = String.create (lines_per_worker * chars_per_line + 2)
    and len = ref (String.length beg) in
    String.blit beg 0 buf 0 !len;
    let process_buffer () =
      let red_len = !len - 1 in
      let mid_point = red_len / 2 in
      for i = 0 to mid_point do
        let ri = red_len - i and tmp = buf.[i] in
        buf.[i] <- rc_table.[Char.code buf.[ri]];
        buf.[ri] <- rc_table.[Char.code tmp]
      done
    and write_by_cols rem eol =
      let len = !len and dne = ref 0 in
      if rem > 0 then begin
        let to_do = min rem (len - !dne) in
        output stdout buf !dne to_do;
        output_char stdout ʼ\nʼ;
        dne := !dne + to_do
      end;
      while len - !dne >= chars_per_line do
        output stdout buf !dne chars_per_line;
        output_char stdout ʼ\nʼ;
        dne := !dne + chars_per_line
      done;
      let rem = len - !dne in
      if rem > 0 then begin
        output stdout buf !dne rem;
        if eol then
          output_char stdout ʼ\nʼ
      end;
      flush stdout;
      if eol then
        0
      else
        rem in
    try
      for i = 2 to lines_per_worker do
        really_input stdin buf !len aug_chars_per_line;
        let new_len = ref (!len + chars_per_line) in
        if buf.[!len] = ʼ>ʼ || buf.[!new_len] <> ʼ\nʼ then begin
          while buf.[!len] <> ʼ>ʼ do
            incr len
          done;
          let ptr = ref !len in
          (* Needed to patch the hideous bug in the output of the C program *)
          if buf.[!len - 1] <> ʼ\nʼ then begin
            String.blit buf !len buf (!len + 1) aug_chars_per_line;
            buf.[!len] <- ʼ\nʼ;
            incr new_len;
            incr ptr
          end else
            decr len;
          while !ptr < !new_len && buf.[!ptr] <> ʼ\nʼ do
            incr ptr
          done;
          match Unix.fork () with
          | 0 ->
              let aug_len = !len + 1 in
              if !ptr = !new_len then
                spawn
                  (String.sub buf
                    aug_len (!new_len - aug_len) ^ input_line stdin)
                  "" true
              else
                let aug_ptr = !ptr + 1 in
                spawn
                  (String.sub buf aug_len (!ptr - aug_len))
                  (String.sub buf aug_ptr (!new_len - !ptr) ^ input_line stdin)
                  true
          | _ ->
              get_ack ();
              output_tag ();
              process_buffer ();
              let rem = write_by_cols 0 first in
              if first then
                put_ack ();
              exit rem
        end;
        len := !new_len
      done;
      match Unix.fork () with
      | 0 -> spawn tag "" false
      | pid ->
          process_buffer ();
          match Unix.waitpid [] pid with
          | _, Unix.WEXITED rem ->
              let rem = write_by_cols (chars_per_line - rem) first in
              if first then
                put_ack ();
              exit rem
          | _ -> assert false
    with End_of_file ->
      while buf.[!len] <> ʼ\nʼ do
        incr len
      done;
      get_ack ();
      put_end_ack ();
      output_tag ();
      process_buffer ();
      let rem = write_by_cols 0 first in
      if first then
        put_ack ();
      exit rem in
  match Unix.fork () with
  | 0 ->
      put_ack ();
      spawn (read_line ()) "" true
  | _ ->
      get_end_ack ();
      get_ack ();
      exit 0

