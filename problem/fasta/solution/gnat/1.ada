-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
--
-- Contributed by Pascal Obry on 2005/04/07

package Fasta_Pck is

   type Real is new Long_Float;

   type Aminoacid is record
      C : Character;
      P : Real;
   end record;

   type Aminoacid_Set is array (Positive range <>) of Aminoacid;

   procedure Make_Cumulative (Gene_List : in out Aminoacid_Set);

   function Select_Random (Gene_List : in Aminoacid_Set) return Character;

   procedure Make_Random_Fasta
     (Id, Desc : in String; Gene_List : in Aminoacid_Set; N : in Positive);

   procedure Make_Repeat_Fasta (Id, Desc, S : in String; N : in Positive);

end Fasta_Pck;

with Ada.Text_IO; use Ada.Text_IO;

package body Fasta_Pck is

   Line_Length : constant := 60;

   Last        : Natural := 42;

   function Gen_Random (Max : in Real) return Real;
   pragma Inline (Gen_Random);

   function Gen_Random (Max : in Real) return Real is
      IM : constant := 139_968;
      IA : constant :=   3_877;
      IC : constant :=  29_573;
   begin
      Last := (Last * IA + IC) mod IM;
      return (Max * Real (Last)) / Real (IM);
   end Gen_Random;

   procedure Make_Cumulative (Gene_List : in out Aminoacid_Set) is
      C : Real := Gene_List (Gene_ListʼFirst).P;
   begin
      for K in Gene_ListʼFirst + 1 .. Gene_ListʼLast loop
         C := C + Gene_List (K).P;
         Gene_List (K).P := C;
      end loop;
   end Make_Cumulative;

   function Select_Random (Gene_List : in Aminoacid_Set) return Character is
      R         : Real := Gen_Random (1.0);
      I, Lo, Hi : Integer;
   begin
      if R < Gene_List (Gene_ListʼFirst).P then
         return Gene_List (Gene_ListʼFirst).C;
      end if;

      Lo := 0;
      Hi := Gene_ListʼLast;

      while Hi > Lo + 1 loop
         I := (Hi + Lo) / 2;
         if R < Gene_List (I).P then
            Hi := I;
         else
            Lo := I;
         end if;
      end loop;

      return Gene_List (Hi).C;
   end Select_Random;

   procedure Make_Random_Fasta
     (Id, Desc : in String; Gene_List : in Aminoacid_Set; N : in Positive)
   is
      Todo : Integer := N;
      M    : Integer;
      Pick : String (1 .. Line_Length);
   begin
      Put_Line (">" & Id & ʼ ʼ & Desc);

      while Todo > 0 loop
         M := NaturalʼMin (Todo, Line_Length);

         for K in 1 .. M loop
            Pick (K) := Select_Random (Gene_List);
         end loop;

         Put_Line (Pick (1 .. M));
         Todo := Todo - Line_Length;
      end loop;
   end Make_Random_Fasta;

   procedure Make_Repeat_Fasta (Id, Desc, S : in String; N : in Positive) is
      Todo : Integer := N;
      K    : Positive := SʼFirst;
      M    : Natural;
   begin
      Put_Line (">" & Id & ʼ ʼ & Desc);

      while Todo > 0 loop
         M := NaturalʼMin (Todo, Line_Length);

         while M >= SʼLength - K + SʼFirst loop
            Put (S (K .. SʼLast));
            M := M - (SʼLength - K + SʼFirst);
            K := SʼFirst;
         end loop;

         Put_Line (S (K .. K + M - SʼFirst));
         K := K + M - SʼFirst + 1;

         Todo := Todo - Line_Length;
      end loop;
   end Make_Repeat_Fasta;

end Fasta_Pck;

with Ada.Command_Line; use Ada.Command_Line;
with Fasta_Pck;        use Fasta_Pck;

procedure Fasta is
   Homosapiens : Aminoacid_Set :=
                   ((ʼaʼ, 0.3029549426680), (ʼcʼ, 0.1979883004921),
                    (ʼgʼ, 0.1975473066391), (ʼtʼ, 0.3015094502008));
   Iub         : Aminoacid_Set :=
                   ((ʼaʼ, 0.27), (ʼcʼ, 0.12), (ʼgʼ, 0.12), (ʼtʼ, 0.27),
                    (ʼBʼ, 0.02), (ʼDʼ, 0.02), (ʼHʼ, 0.02), (ʼKʼ, 0.02),
                    (ʼMʼ, 0.02), (ʼNʼ, 0.02), (ʼRʼ, 0.02), (ʼSʼ, 0.02),
                    (ʼVʼ, 0.02), (ʼWʼ, 0.02), (ʼYʼ, 0.02));
   Alu         : constant String :=
                    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" &
                    "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" &
                    "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" &
                    "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" &
                    "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" &
                    "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" &
                    "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

   N           : constant Positive := PositiveʼValue (Argument (1));

begin
   Make_Cumulative (Iub);
   Make_Cumulative (Homosapiens);

   Make_Repeat_Fasta ("ONE", "Homo sapiens alu", alu, N * 2);
   Make_Random_Fasta ("TWO", "IUB ambiguity codes", Iub, N * 3);
   Make_Random_Fasta
     ("THREE", "Homo sapiens frequency", Homosapiens, N * 5);
end Fasta;

