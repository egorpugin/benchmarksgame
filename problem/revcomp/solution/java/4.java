/*
 * The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 * contributed by Anthony Donnefort
 * slightly modified to read 82 bytes at a time by Razii
 */

import java.io.*;
public class revcomp {
   static final byte[] cmp = new byte[128];
   static {
      for (int i = 0; i < cmp.length; i++) cmp[i] = (byte) i;
      cmp[ʼtʼ] = cmp[ʼTʼ] = ʼAʼ;
      cmp[ʼaʼ] = cmp[ʼAʼ] = ʼTʼ;
      cmp[ʼgʼ] = cmp[ʼGʼ] = ʼCʼ;
      cmp[ʼcʼ] = cmp[ʼCʼ] = ʼGʼ;
      cmp[ʼvʼ] = cmp[ʼVʼ] = ʼBʼ;
      cmp[ʼhʼ] = cmp[ʼHʼ] = ʼDʼ;
      cmp[ʼrʼ] = cmp[ʼRʼ] = ʼYʼ;
      cmp[ʼmʼ] = cmp[ʼMʼ] = ʼKʼ;
      cmp[ʼyʼ] = cmp[ʼYʼ] = ʼRʼ;
      cmp[ʼkʼ] = cmp[ʼKʼ] = ʼMʼ;
      cmp[ʼbʼ] = cmp[ʼBʼ] = ʼVʼ;
      cmp[ʼdʼ] = cmp[ʼDʼ] = ʼHʼ;
      cmp[ʼuʼ] = cmp[ʼUʼ] = ʼAʼ;
   }

   static class ReversibleByteArray extends java.io.ByteArrayOutputStream {
      void reverse() throws Exception {
         if (count > 0) {
            int begin = 0, end = count - 1;
            while (buf[begin++] != ʼ\nʼ);
            while (begin <= end) {
               if (buf[begin] == ʼ\nʼ) begin++;
               if (buf[end] == ʼ\nʼ) end--;
               if (begin <= end) {
                  byte tmp = buf[begin];
                  buf[begin++] = cmp[buf[end]];
                  buf[end--] = cmp[tmp];
               }
            }
            System.out.write(buf, 0, count);
         }
      }
   }

   public static void main(String[] args) throws Exception {
      byte[] line = new byte[82];
      int read;
      ReversibleByteArray buf = new ReversibleByteArray();
      while ((read = System.in.read(line)) != -1) {
         int i = 0, last = 0;
         while (i < read) {
            if (line[i] == ʼ>ʼ) {
               buf.write(line, last, i - last);
               buf.reverse();
               buf.reset();
               last = i;
            }
            i++;
         }
         buf.write(line, last, read - last);
      }
      buf.reverse();
   }
}



