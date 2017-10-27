-- lists

with Ada.Text_IO, Ada.Integer_Text_IO,Ada.Numerics.discrete_Random,Ada.Unchecked_Deallocation;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure Lab4List is

type Element is
  record
    Data : Integer := 0;
    Next : access Element := Null;
  end record;

type Elem_Ptr is access all Element;

procedure Print(List : access Element) is
  L : access Element := List;
begin
  if List = Null then
    Put_Line("List EMPTY!");
  else
    Put_Line("List:");
  end if;
  while L /= Null loop
    Put(L.Data, 4); -- z pakietu Ada.Integer_Text_IO
    New_Line;
    L := L.Next;
  end loop;
end Print;

procedure Insert(List : in out Elem_Ptr; D : in Integer) is
  E : Elem_Ptr := new Element;
begin
  E.Data := D;
  E.Next := List;
  -- lub E.all := (D, List);
  List := E;
end Insert;

function Insert(List : access Element; D : in Integer) return access Element is
  ( new Element'(D,List) );

-- Insert element asc
procedure Insert_Sort(List : in out Elem_Ptr; D : in Integer) is
	L : access Element := List;
	NewEl : access Element;
begin
  	L := List;
  	if List = Null then
		L := new Element'(D, Null);
  	elsif L.Data > D then
	  	List := new Element'(D, L);
  	else
	  	while  L.Next /= Null and then L.Next.Data < D loop
		  	L := L.Next;
	 	end loop;
	  	NewEl := new Element'(D,L.Next);
	  	L.Next := NewEl;
  	end if;
end Insert_Sort;

procedure Random_Insert (List : in out Elem_Ptr; M,N : in Positive) is
	E : Elem_Ptr := new Element;
	L : access Element := List;
	subtype Rand_Range is Integer range 0..M;
	package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
	use Rand_Int;
	Gen : Rand_Int.Generator;
	Num : Rand_Range;
begin
	Rand_Int.Reset(Gen);
        for I in 1..N loop
        	Num := Random(Gen); --randomized integer
        	Insert_Sort(List,Num);
        end loop
end Random_Insert;

--search
function Search(List : in Elem_Ptr; El : in Integer) return Boolean is
	L : access Element := List;
begin
	while L /= Null loop
		if L.Data = El then
			return True;
		end if;
		L := L.Next;
	end loop;
	return False;
end Search;

procedure Free is new Ada.Unchecked_Deallocation(Element, Elem_Ptr);

--Remove first element from list
procedure RemoveFirst (List : in out Elem_Ptr) is
	L: access Element := List;
   	Temp : access Element;
begin
	if List /= Null then
		L := List;
		List := L.Next;
		Free(L);
    	end if;
end RemoveFirst;

--Remove last element from list
procedure RemoveLast (List : in out Elem_Ptr) is
    L: access Element := List;
    Temp : access Element;
begin
    L := List;
    if List /= Null then
        while L.Next /= Null loop
            Temp := L;
            L := L.Next;
        end loop;
        Free(L);
        --L.Next := Null;
        L := Temp;
        L.Next := Null;
    end if;
end RemoveLast;

--Remove element from list
procedure Remove(List: in out Elem_Ptr; Elem : in Integer) is
	L : access Element := List;
	Temp : access Element;
begin
	Temp := L;
	if L /= Null then
		if Search(List,Elem) = False then
		   Put_Line("Element is not in the list");
		   return;
		end if;

		if L.Data = Elem then
		     RemoveFirst(List);           --The element to remove is first in the list
		else
			while L.Next /= Null loop
				if L.Next.Data = Elem then
					Temp := L.Next.Next;      --The element is inside the list
					Free(L.Next);
					L.Next := Temp;
					return;
				end if;
				L := L.Next;
			end loop;
			RemoveLast(List);		--The element is at the end of list
		end if;
   	end if;
end Remove;

-- Main
Lista : Elem_Ptr := Null;

begin

  Print(Lista);
  Lista := Insert(Lista, 21);
  Print(Lista);
  Insert(Lista, 20);
  Insert_Sort(Lista,7);
  Print(Lista);
  
  Put_Line("Insert random elements");
  Random_Insert(Lista,10,2);
  Print(Lista);
  Put_Line("Is 20 in List? " & Search(Lista,20)'Img);
  
  Insert_Sort(Lista,0);
  RemoveFirst(Lista);
  RemoveLast(Lista);
  Remove(Lista,7);
  Print(Lista);

end Lab4List;
