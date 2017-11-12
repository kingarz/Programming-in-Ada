-- Tree

with Ada.Text_IO, Ada.Integer_Text_IO,Ada.Numerics.discrete_Random,Ada.Unchecked_Deallocation;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure Tree is

   type Node is
      record
         Data : Integer := 0;
         Left : access Node := Null;
         Right: access Node := Null;
      end record;

   type Node_Ptr is access all Node;


   -- Print the tree
   procedure Print(Tree : Node_Ptr) is
   begin
      if Tree = Null then
         Put_Line("The tree is empty");
         return;
      elsif Tree.Left = Null and Tree.Right = Null then
         Put(Tree.Data,0);
      else
         Put(Tree.Data,0);
         Put('(');
         Print(Tree.Left);
         Put(',');
         Print(Tree.Right);
         Put(')');
      end if;
      New_Line;
   end Print;


   --  insert element into tree - bst without duplicates
   procedure InsertBST(Tree : in out Node_Ptr; Key : Integer) is
   begin
      --insert into an empty tree
      if Tree = Null then
         Tree := new Node'(Key,Null,Null);

      elsif Key < Tree.Data then
         if Tree.Left = Null then
            Tree.Left := new Node'(Key,Null,Null);

         else
            InsertBST(Tree.Left, Key);
         end if;

      elsif Key > Tree.Data then
         if Tree.Right = Null then
            Tree.Right := new Node'(Key,Null,Null);
         else
            InsertBST(Tree.Right, Key);
         end if;

      end if;
   end InsertBST;

   --random insert
   procedure RandomInsert (Tree : in out Node_Ptr; N,M : in Positive) is
      subtype Rand_Range is Integer range 0..M;
      package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
      use Rand_Int;
      Gen : Rand_Int.Generator;
      Num : Rand_Range;
   begin
      Reset(Gen);
      for I in 1..N loop
         Num := Random(Gen) mod M; --randomized integer
         InsertBST(Tree,Num);
      end loop;
   end RandomInsert;


   --search the value(key) in the tree
   function Search (Tree : in out Node_Ptr; Key : in Integer) return Boolean is
      T : access Node := Tree;
   begin
      while T /= Null loop
         if T.Data = Key then
            return True;
         end if;
         if T.Data > Key then
            T := T.Left;
         else
            T := T.Right;
         end if;
      end loop;
      return False;
   end Search;



   procedure Free is new Ada.Unchecked_Deallocation(Node, Node_Ptr);
   --remove the node that has data = key
   procedure RemoveNode(Tree : in out Node_Ptr; Key : in Integer) is
      T : access Node :=  Tree;
      Tmp : access Node := Tree;
      Parent : access Node := Tree;
      IsChildLeft : Boolean := False;
   begin
      if Tree = Null then
         Put_Line("The tree is empty!");
         return;
      elsif Search(Tree,Key) = False then
         Put_Line("The Key is not in the Tree");
         return;
      end if;
      --the node hasn't got children,just remove that node
      if T.Left = Null and T.Data = Key and T.Right = Null then
         Free(T);
         return;
      end if;
      --nodes have children
      while T /= Null loop
         if Key < T.Data then
            Parent := T;
            T := T.Left;
            IsChildLeft := True;
         elsif Key > T.Data then
            Parent := T;
            T:= T.Right;
            IsChildLeft := False;
         end if;
         if T.Data = Key then    --the node that will be removed
            if T.Left = Null and T.Right = Null then    --the node hasn't got children
               if IsChildLeft then
                  Parent.Left := Null;
               else
                  Parent.Right := Null;
               end if;
               Free(T);
               return;
            elsif T.Left /= Null and T.Right = Null then   --the node has got left child

               Tmp := T.Left;
               Free(T);
               T := Tmp;
               if IsChildLeft then
                  Parent.Left := Tmp;
               else
                  Parent.Right := Tmp;
               end if;
               return;
            elsif T.Right /= Null and T.Left = Null then     --the node has got right child
               Tmp := T.Right;
               Free(T);
               T := Tmp;
               if IsChildLeft then
                  Parent.Left := Tmp;
               else
                  Parent.Right := Tmp;
               end if;
               return;
            else
               -- the node has got both children
               Tmp := T.Right;   -- the smallest value from right subtree
               while Tmp.Left /= Null loop
                  Tmp := Tmp.Left;
               end loop;
               Tmp.Left := T.Left;
               if Tmp /= T.Right then
                  Tmp.Right := T.Right;
               else
                  Tmp.Right := Null;
               end if;
               if IsChildLeft then
                  Parent.Left := Tmp;
               else
                  Parent.Right := Tmp;
               end if;
               Free(T);
               return;
            end if;
         end if;
      end loop;

   end RemoveNode;

   --Tree as xml file - ugly syntax of file :p
   procedure RecSaveAsXml (Tree : in out Node_Ptr; File : in out File_Type) is
      T : access Node := Tree;
      T2 : access Node := Tree;
   begin
      if Tree = Null then
         return;
      else
         if Tree.Left /= Null then
            Put_Line(File, "   <leftNode>" & Tree.Left.Data'Img);
            RecSaveAsXml(Tree.Left, File);
            Put_Line(File, "   </leftNode>");
         end if;
         if Tree.Right /= Null then
            Put_Line(File,"   <rightNode>" & Tree.Right.Data'Img);
            RecSaveAsXml(Tree.Right,File);
            Put_Line(File,"   </rightNode>");
         end if;
      end if;
   end RecSaveAsXml;

   procedure SaveAsXml (Tree : in out Node_Ptr) is
      T : access Node := Tree;
      File : File_Type;
      Name: String := "tree.xml";
   begin
      Create(File, Out_File, Name);
      Put_Line(File, "<?xml?>");
      Put_Line(File, " <tree>");
      Put_Line(File, "  <Head>" & T.Data'Img);

      if Tree = Null then
         Put_Line(File, " </tree>");
         return;
      end if;

      RecSaveAsXml(Tree,File);
      Put_Line(File, "  </Head>");
      Put_Line(File, " </tree>");

   end SaveAsXml;

   Tree : Node_Ptr := Null;
   T2 : Node_Ptr := Null;

begin

   InsertBST(T2,5);
   InsertBST(Tree,8);
   InsertBST(Tree,2);
   InsertBST(Tree,4);
   InsertBST(Tree,1);
   SaveAsXml(Tree);

   --RandomInsert(Tree,2,10);
   --Print(Tree);

   Print(Tree);
   RemoveNode(Tree,2);
   Print(Tree);
   --RemoveNode(Tree2,10);

end Tree;
