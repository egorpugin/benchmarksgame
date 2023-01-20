--
--  The Computer Language Benchmarks Game
--  https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
--
-- Contributed by Pascal Obry on 2005/03/19
-- Optimized by Bill Findlay on 2005/04/04

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

procedure Revcomp is

   Complement : constant array (Character) of Character :=
                  (ʼAʼ => ʼTʼ, ʼCʼ => ʼGʼ, ʼGʼ => ʼCʼ, ʼTʼ => ʼAʼ, ʼUʼ => ʼAʼ,
                   ʼMʼ => ʼKʼ, ʼRʼ => ʼYʼ, ʼWʼ => ʼWʼ, ʼSʼ => ʼSʼ, ʼYʼ => ʼRʼ,
                   ʼKʼ => ʼMʼ, ʼVʼ => ʼBʼ, ʼHʼ => ʼDʼ, ʼDʼ => ʼHʼ, ʼBʼ => ʼVʼ,
                   ʼNʼ => ʼNʼ,
                   ʼaʼ => ʼTʼ, ʼcʼ => ʼGʼ, ʼgʼ => ʼCʼ, ʼtʼ => ʼAʼ, ʼuʼ => ʼAʼ,
                   ʼmʼ => ʼKʼ, ʼrʼ => ʼYʼ, ʼwʼ => ʼWʼ, ʼsʼ => ʼSʼ, ʼyʼ => ʼRʼ,
                   ʼkʼ => ʼMʼ, ʼvʼ => ʼBʼ, ʼhʼ => ʼDʼ, ʼdʼ => ʼHʼ, ʼbʼ => ʼVʼ,
                   ʼnʼ => ʼNʼ,
                   others => ʼ?ʼ);

   Max_Length : constant := 60;

   type String_Access is access String;

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Get_Fasta (Seq : in out String_Access; Length : out Natural) is
      Ptr  : String_Access;
      J, K : Natural := 0;
      C    : Character;
      EOL  : Boolean;
   begin
      Length := 0;

      loop
         Look_Ahead (C, EOL);
         exit when C /= ʼ>ʼ and C /= ʼ;ʼ ;
         Get_Line (Seq (1 .. SeqʼLength), J);
         if Seq (1) = ʼ>ʼ then
            Put_Line (Seq (1 .. J));
         end if;
      end loop;

      loop
         if Length + Max_Length > SeqʼLength then
            Ptr := Seq;
            Seq := new String (1 .. 2 * SeqʼLength);
            Seq (1 .. Length) := Ptr (1 .. Length);
            Free (Ptr);
         end if;

         Get_Line (Seq (Length + 1 .. Length + Max_Length), J);
         K := J - Length;
         Length := J;

         if K = Max_Length then
            Skip_Line;
         end if;

         Look_Ahead (C, EOL);

         if K < Max_Length or else C = ʼ>ʼ or else C = ʼ;ʼ then
            return;
         end if;
      end loop;
   exception
      when End_Error =>
         return;
   end Get_Fasta;

   procedure Reverse_Fasta (Fasta : in out String) is
      C    : Character;
      L, R : Natural;
   begin
      for I in 0 .. FastaʼLength loop
         L := FastaʼFirst + I;
         R := FastaʼLast - I;
         exit when L > R;
         C := Fasta (L);
         Fasta (L) := Complement (Fasta (R));
         Fasta (R) := Complement (C);
      end loop;
   end Reverse_Fasta;

   procedure Put_Reversed_Fasta (Fasta : in out String) is
      L : Natural := FastaʼLast;
   begin
      while L >= Max_Length loop
         Reverse_Fasta (Fasta (L - Max_Length + 1 .. L));
         Put_Line (Fasta (L - Max_Length + 1 .. L));
         L := L - Max_Length;
      end loop;
      if L > 0 then
         Reverse_Fasta (Fasta (1 .. L));
         Put_Line (Fasta (1 .. L));
      end if;
   end Put_Reversed_Fasta;

   Seq    : String_Access := new String (1 .. 1_024);
   Length : Natural;

begin
   loop
      Get_Fasta (Seq, Length);
      exit when Length = 0;
      Put_Reversed_Fasta (Seq (1 .. Length));
   end loop;
end Revcomp;

