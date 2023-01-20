/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by The Go Authors.
 */

package main

import (
   "bufio"
   "os"
)

const lineSize = 60

var complement = [256]uint8{
   ʼAʼ: ʼTʼ, ʼaʼ: ʼTʼ,
   ʼCʼ: ʼGʼ, ʼcʼ: ʼGʼ,
   ʼGʼ: ʼCʼ, ʼgʼ: ʼCʼ,
   ʼTʼ: ʼAʼ, ʼtʼ: ʼAʼ,
   ʼUʼ: ʼAʼ, ʼuʼ: ʼAʼ,
   ʼMʼ: ʼKʼ, ʼmʼ: ʼKʼ,
   ʼRʼ: ʼYʼ, ʼrʼ: ʼYʼ,
   ʼWʼ: ʼWʼ, ʼwʼ: ʼWʼ,
   ʼSʼ: ʼSʼ, ʼsʼ: ʼSʼ,
   ʼYʼ: ʼRʼ, ʼyʼ: ʼRʼ,
   ʼKʼ: ʼMʼ, ʼkʼ: ʼMʼ,
   ʼVʼ: ʼBʼ, ʼvʼ: ʼBʼ,
   ʼHʼ: ʼDʼ, ʼhʼ: ʼDʼ,
   ʼDʼ: ʼHʼ, ʼdʼ: ʼHʼ,
   ʼBʼ: ʼVʼ, ʼbʼ: ʼVʼ,
   ʼNʼ: ʼNʼ, ʼnʼ: ʼNʼ,
}

func main() {
   in := bufio.NewReader(os.Stdin)
   buf := make([]byte, 1024*1024)
   line, err := in.ReadSlice(ʼ\nʼ)
   for err == nil {
      os.Stdout.Write(line)

      // Accumulate reversed complement in buf[w:]
      nchar := 0
      w := len(buf)
      for {
         line, err = in.ReadSlice(ʼ\nʼ)
         if err != nil || line[0] == ʼ>ʼ {
            break
         }
         line = line[0 : len(line)-1]
         nchar += len(line)
         if len(line)+nchar/60+128 >= w {
            nbuf := make([]byte, len(buf)*5)
            copy(nbuf[len(nbuf)-len(buf):], buf)
            w += len(nbuf) - len(buf)
            buf = nbuf
         }

         // This loop is the bottleneck.
         for _, c := range line {
            w--
            buf[w] = complement[c]
         }
      }

      // Copy down to beginning of buffer, inserting newlines.
      // The loop left room for the newlines and 128 bytes of padding.
      i := 0
      for j := w; j < len(buf); j += 60 {
         n := copy(buf[i:i+60], buf[j:])
         buf[i+n] = ʼ\nʼ
         i += n + 1
      }
      os.Stdout.Write(buf[0:i])
   }
}

