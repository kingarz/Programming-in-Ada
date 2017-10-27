with Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Text_IO, Ada.Calendar;
use Ada.Text_IO, Ada.Numerics.Float_Random, Ada.Calendar;

procedure Vector is 
   type Vector is array (Integer range <>) of Float;
   V1 : Vector (1..10 ):= (1=> 8.0, 2=> 0.5, 3=> 3.0, 4=>0.0, 5=> 3.5, 6=>3.0, others => 1.0);
   VTest : Vector (1..5) := (others=> 0.0);
   G : Ada.Numerics.Float_Random.Generator;
   StartTime,EndTime : Time;
   Dur : Duration;
   
   
   procedure PrintVector(V : Vector) is
   begin
      Put_Line("Wypisywanie wektora");
      for I in V'range loop
         Put(V(I)'Img & "; ");
         New_Line;
      end loop;
      New_Line;
   end PrintVector;
   
   --Vector with random values
   function RandomVector(Size : Integer) return Vector is
      NewV : Vector (1..Size) := (others => 0.0);
   begin
      for I in NewV'range loop
         NewV(I) := Ada.Numerics.Float_Random.Random(G);
      end loop;
      return NewV;
   end RandomVector;
   
   --Is sorted ASC?
   function IsSorted(V : Vector) return Boolean is
   begin
      return (for all I in V'First..(V'Last-1) => V(I)<=V(I+1));
   end IsSorted;

 
   function BubbleSort(V: in out Vector) return Vector is
      Temp : Float;
   begin
      for I in reverse V'Range loop
         for J in V'First .. I loop
            if V(I) < V(J) then
               Temp := V(J);
               V(J) := V(I);
               V(I) := Temp;
            end if;
         end loop;
      end loop;
      return V;
   end BubbleSort;

   --Save vector in file
   procedure VectorToFile (V: Vector) is
      File : File_Type;
      Nazwa: String := "vector.txt";	
   begin
      Create(File, Out_File, Nazwa); -- Open
      
      for I in V'Range loop
         Put_Line(File,"Index :" & I'Img & " value : "  & V(I)'Img);
      end loop;
      
      Close(File);
   end VectorToFile;
   
begin
   
   StartTime := Clock;
   
   Put_Line("Random vector before sorting... ");
   VTest := RandomVector(5);
   PrintVector(VTest);
   Put_Line("After bubble sort");
   PrintVector(BubbleSort(VTest));
   Put_Line("Is is sorted now? " & IsSorted(BubbleSort(VTest))'Img);
   
   EndTime := Clock;
   Dur := EndTime - StartTime;
   Put_Line("Execution time = " & Dur'Img & "[s]");
   
   VectorToFile(V1);
   
end Vector; 
