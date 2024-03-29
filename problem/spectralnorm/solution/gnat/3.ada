-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
--
-- Contributed by Jim Rogers
-- Modified by Jonathan Parker (Oct 2009)

pragma Restrictions (No_Abort_Statements);
pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);

with Ada.Text_Io;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Command_Line; use Ada.Command_Line;
with Spectral_Utils;

procedure SpectralNorm is

   type Real is digits 15;

   No_of_Cores_to_Use : constant := 4;

   package Real_IO is new Ada.Text_Io.Float_Io(Real);
   package Real_Funcs is new Ada.Numerics.Generic_Elementary_Functions(Real);
   use Real_Funcs;

   N : Natural := 100;
   Vbv, Vv : Real := 0.0;
begin
   if Argument_Count = 1 then
      N := Natural'Value (Argument(1));
   end if;

   declare
      package Spectral_Utilities is new Spectral_Utils
        (Real, No_of_Tasks => No_of_Cores_to_Use, Matrix_Size => N);
      use Spectral_Utilities;

      U : Matrix := (Others => 1.0);
      V : Matrix := (Others => 0.0);
   begin
      for I in 1 .. 10 loop
         Eval_Ata_Times_U(U, V);
         Eval_Ata_Times_U(V, U);
      end loop;
      for I in V'Range loop
         Vbv := Vbv + U(I) * V(I);
         Vv  := Vv  + V(I) * V(I);
      end loop;
   end;
   Real_IO.Put(Item => Sqrt(Vbv/Vv), Fore => 1, Aft => 9, Exp => 0);
   Ada.Text_Io.New_Line;
end SpectralNorm;

generic

   type Real is digits <>;
   No_Of_Tasks : Positive;
   Matrix_Size : Positive;

package Spectral_Utils is

   type Matrix is array(Natural range 0 .. Matrix_Size-1) of Real;

   --  Evaluate matrix A at indices I, J.

   function Eval_A(I, J : Natural) return Real;

   --  Get   A_transpose_A_times_U = A_transpose * A * U.

   procedure Eval_Ata_Times_U
     (U                     : in Matrix;
      A_transpose_A_times_U : out Matrix);

   --  Get   AU = A * U.  Calculate only AU(Start .. Finish).

   procedure Eval_A_Times
     (U      : in  Matrix;
      Start  : in  Natural;
      Finish : in  Natural;
      AU     : out Matrix);

   --  Get   AU = A_transpose * U.   Calculate only AU(Start .. Finish).

   procedure Eval_At_Times
     (U      : in  Matrix;
      Start  : in  Natural;
      Finish : in  Natural;
      AU     : out Matrix);

   pragma Inline (Eval_A_Times, Eval_At_Times);
   pragma Inline (Eval_A, Eval_Ata_Times_U);

end Spectral_Utils;

package body Spectral_Utils is

   function Eval_A (I, J : in Natural) return Real is
      Denom : constant Real := Real (((I + J) * (I + J + 1)) / 2 + I + 1);
   begin
      return 1.0 / Denom;
   end Eval_A;

   type A_Element_Pair is array (0 .. 1) of Real;

   -- Evaluate matrix A twice - at (I,J) and (I,J+1):

   function Eval_A_Twice (I, J : in Integer) return A_Element_Pair is
      Denom_0 : constant Real := Real ((I + J    )*(I + J + 1)/2 + I + 1);
      Denom_1 : constant Real := Real ((I + J + 1)*(I + J + 2)/2 + I + 1);
   begin
      return (1.0 / Denom_0, 1.0 / Denom_1);
   end Eval_A_Twice;

   -- Evaluate A_transpose (indices I and J swapped):

   function Eval_A_tr_Twice (I, J : in Integer) return A_Element_Pair is
      Denom_0 : constant Real := Real ((I + J    )*(I + J + 1)/2 + J + 1);
      Denom_1 : constant Real := Real ((I + J + 1)*(I + J + 2)/2 + J + 2);
   begin
      return (1.0 / Denom_0, 1.0 / Denom_1);
   end Eval_A_tr_Twice;

   procedure Eval_A_Times
     (U      : in  Matrix;
      Start  : in  Natural;
      Finish : in  Natural;
      Au     : out Matrix)
   is
      Sum : Real;
      J_Index : Natural;
      A_Elements : A_Element_Pair;
   begin
      for I in Start .. Finish loop
         Sum := 0.0;
         for J in Natural range 0 .. U'Length/2 - 1 loop
            J_Index    := U'First + 2*J;
            A_Elements := Eval_A_Twice (I, J_Index);
            Sum := Sum + A_Elements(0)*U(J_Index) + A_Elements(1)*U(J_Index+1);
         end loop;
         if U'Length mod 2 = 1 then
            Sum := Sum + Eval_A(I, U'Last) * U(U'Last); -- J_Index := U'Last;
         end if;
         Au(I) := Sum;
      end loop;
   end Eval_A_Times;


   procedure Eval_At_Times
     (U      : in  Matrix;
      Start  : in  Natural;
      Finish : in  Natural;
      Au     : out Matrix)
   is
      Sum : Real;
      J_Index : Natural;
      A_Elements : A_Element_Pair;
   begin
      for I in Start .. Finish loop
         Sum := 0.0;
         for J in Natural range 0 .. U'Length/2 - 1 loop
            J_Index    := U'First + 2*J;
            A_Elements := Eval_A_tr_Twice (I, J_Index);
            Sum := Sum + A_Elements(0)*U(J_Index) + A_Elements(1)*U(J_Index+1);
         end loop;
         if U'Length mod 2 = 1 then
            Sum := Sum + Eval_A (U'Last, I) * U(U'Last); -- J_Index := U'Last;
         end if;
         Au(I) := Sum;
      end loop;
   end Eval_At_Times;


   -- Calculate A * U

   task type Matrix_A_times_U is
      pragma Storage_Size (2**20);
      entry Multiply (U : in Matrix; Start : in Natural; Finish : in Natural);
      entry Result (Start : out Natural; Finish : out Natural; R : out Matrix);
   end Matrix_A_times_U;

   task body Matrix_A_times_U is
      I1, I2 : Natural;
      AU, U_local : Matrix;
   begin
     loop
     select
        accept Multiply (U : in Matrix; Start : in Natural; Finish : in Natural)
 do
           I1 := Start;
           I2 := Finish;
           U_local := U;
        end Multiply;

        Eval_A_Times (U_local, I1, I2, AU); -- updates AU(I1..I2)

        accept Result (Start : out Natural; Finish : out Natural; R : out Matrix
) do
           Start  := I1;
           Finish := I2;
           R(Start .. Finish) := AU(Start .. Finish);
        end Result;
     or
        terminate;
     end select;
     end loop;
   end Matrix_A_times_U;


   -- Calculate A_transpose * V

   task type Matrix_A_tr_times_V is
      pragma Storage_Size (2**20);
      entry Multiply (V : in Matrix; Start : in Natural; Finish : in Natural);
      entry Result (Start : out Natural; Finish : out Natural; R : out Matrix);
   end Matrix_A_tr_times_V;

   task body Matrix_A_tr_times_V is
      I1, I2 : Natural;
      AV, V_local : Matrix;
   begin
     loop
     select
        accept Multiply (V : in Matrix; Start : in Natural; Finish : in Natural)
 do
           I1 := Start;
           I2 := Finish;
           V_local := V;
        end Multiply;

        Eval_At_Times (V_local, I1, I2, AV);  -- AV = A_transpose * V_local

        accept Result (Start : out Natural; Finish : out Natural; R : out Matrix
) do
           Start  := I1;
           Finish := I2;
           R(Start .. Finish) := AV(Start .. Finish);
        end Result;
     or
        terminate;
     end select;
     end loop;
   end Matrix_A_tr_times_V;


   -- Create (No_Of_Tasks-1) tasks. The final task is the environmental task,
   -- which does its fair share of the work in procedure Eval_Ata_Times_U.

   subtype Task_Range is Positive range 1 .. No_Of_Tasks-1;

   Partial_Matrix_A_times_U    : array (Task_Range) of Matrix_A_times_U;
   Partial_Matrix_A_tr_times_V : array (Task_Range) of Matrix_A_tr_times_V;


   procedure Eval_Ata_Times_U
     (U                     : in  Matrix;
      A_transpose_A_times_U : out Matrix)
   is
      V, Partial_Product : Matrix;

      Segment_Length : constant Integer := U'Length / No_Of_Tasks + 1;
      -- Gives the 1st few tasks a slightly greater share of the work.

      I1, I2, J1, J2 : Natural;
   begin
      I1 := V'First;
      I2 := V'First + Segment_Length - 1;
      I2 := Integer'Min (I2, V'Last);

      -- Start running the tasks in Task_Range:

      for k in Task_Range loop
         Partial_Matrix_A_times_U(k).Multiply (U, I1, I2);
         I1 := I2 + 1;
         I2 := I2 + Segment_Length;
         I2 := Integer'Min (I2, V'Last);
      end loop;

      Eval_A_Times (U, I1, V'Last, V); -- Env task updates V(I1 .. V'Last).

      -- Rendezvous with tasks to get partial results. Write results to V:

      for k in Task_Range loop
         Partial_Matrix_A_times_U(k).Result (J1, J2, Partial_Product);
         V(J1 .. J2) := Partial_Product(J1 .. J2);
      end loop;

      -- The result, stored in V, is A*U. Next get A_transpose * (A*U).

      I1 := V'First;
      I2 := V'First + Segment_Length - 1;
      I2 := Integer'Min (I2, V'Last);

      for k in Task_Range loop
         Partial_Matrix_A_tr_times_V(k).Multiply (V, I1, I2);
         I1 := I2 + 1;
         I2 := I2 + Segment_Length;
         I2 := Integer'Min (I2, V'Last);
      end loop;

      Eval_At_Times (V, I1, V'Last, A_transpose_A_times_U);
      -- Env. task updates A_transpose_A_times_U (I1 .. V'Last).

      for k in Task_Range loop
         Partial_Matrix_A_tr_times_V(k).Result (J1, J2, Partial_Product);
         A_transpose_A_times_U(J1 .. J2) := Partial_Product(J1 .. J2);
      end loop;

   end Eval_Ata_Times_U;

end Spectral_Utils;



