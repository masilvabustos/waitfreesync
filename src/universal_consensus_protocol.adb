
with Ada.Assertions;
use Ada.Assertions;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Universal_Consensus_Protocol is

   protected body Compact_Cell_Fields
   is
      procedure Decrement_Count
      is
      begin

         register := register - 1;
      end Decrement_Count;

      function Count return Natural
      is
      begin
         return register mod (Consensus_Number + 3);
      end Count;


      procedure Decide_After(prefer : in out Integer)
      is
      begin
         if register mod (Consensus_Number + 3) = Consensus_Number + 2
         then
           register
             := register - 1 + Integer (prefer) * (Consensus_Number + 3);
         else
            prefer := (register / (Consensus_Number + 3));
         end if;
      end Decide_After;

   end Compact_Cell_Fields;

  procedure Allocate_From_Subpool (
                                   Pool : in out Cell_Pool;
                                   Storage_Address : out Address;
                                   Size_In_Storage_Elements : in Storage_Elements.Storage_Count;
                                   Alignment : in Storage_Elements.Storage_Count;
                                   Subpool : in not null Subpool_Handle)
   is
      Handle : Private_Pool_Handle := Private_Pool_Handle (Subpool);
      Cell : UCO.Cell;
   begin
      case Policy is
         when GENERAL =>
            declare
               N : Cell_Record_Pool_Index
                 renames Consensus_Number;
               L : constant Cell_Record_Pool_Index
                 := N*(N +1);
               S : constant Integer
                 :=
                   + Process'Pos (Handle.P)
                 - Process'Pos (Process'First);
               subtype X is
                 Cell_Record_Pool_Index range
                   Cell_Record_Pool_Index'First + S * L
                     ..  Cell_Record_Pool_Index'First + (S+1) * L ;
            begin

               for I in X loop
                  Cell := UCO.Pool(I) 'Access;
                  exit when Cell.count = 0;
               end loop;
               Assert (Cell.count = 0, "Did not find cell. This shouldn't happen (1).");
               Storage_Address := Cell.all'Address;
            end;
         when LIFO =>
            null;
      end case;
   end Allocate_From_Subpool;

   function Pool_of_Subpool
     (Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_With_Subpools'Class
   is
   begin
      return UCO.My_Cell_Pool'Access;
   end Pool_of_Subpool;


   function Create_Subpool (Pool : in out Cell_Pool)
                              return not null Subpool_Handle
   is
   begin

      raise Storage_Error; --All subpools are created statically.
      return Default_Subpool_For_Pool (Pool);

   end Create_Subpool;

   procedure Reset (cell : in out Cell_Record)
   is
      use Cell_Consensus_Protocol;
      use Update_Consensus_Protocol;
   begin
      reset (cell.after);
      reset (cell.update);
      cell.count := Atomic_Unsigned (Consensus_Number + 1);
      cell.seq := 0;
   end Reset;

   procedure Reset_Cell (cell : in Universal_Consensus_Protocol.Cell)
   is

   begin
      Reset (cell.all);
   end Reset_Cell;

   procedure Set_Reachability (cell : in out Cell_Record; r : Integer)
   is
   begin
      cell.count := Atomic_Unsigned (r);
   end Set_Reachability;


   procedure Decrement_Reachability (cell : in out Cell_Record)
   is
   begin
      Decrement (cell.count);

   end Decrement_Reachability;

   procedure Decrement_Reachability (cell : in UCO.Cell)
   is
   begin
      Decrement_Reachability (cell.all);
   end Decrement_Reachability;

   procedure Set_Before(cell : in out Cell_Record;
                        before : Cell_Record_Pool_Index)
   is
      pragma Compile_Time_Warning (true, "Set Before not fully implemented.");
   begin
      cell.before := Pool (before) 'Access;
   end Set_Before;

   package body Update_Consensus_Protocol is

      procedure Free is new Ada.Unchecked_Deallocation (Object => Update, Name => Value);


      protected body Consensus_Object
      is
         procedure decide (prefer : in out Long_Integer)
         is

         begin
            if decision = Undecided then
               decision := prefer;
            else
               prefer := decision;
            end if;
         end decide;

         function x return Value
         is
         begin
            return To_Value(decision);
         end x;

         procedure reset
         is
         begin
            decision := Undecided;
         end reset;


      end Consensus_Object;



      procedure reset (object : in out Consensus_Object)
      is
      begin
         object.reset;
      end reset;

      procedure decide (object : in out Consensus_Object; prefer : Update)
      is
         val : Value := new Update'(prefer);
         decision : Long_Integer := To_Long_Integer (val);
       begin
         object.decide(decision);
         if To_Value(decision) /= val then
            Free (val);
         end if;

         end decide;

   end Update_Consensus_Protocol;

   use Update_Consensus_Protocol;

   function Apply (inv : Invocation; s : Operations.State)
                    return Update
   is
   begin
      return x : Update do
         x.state := s;
         x.result := Operations.Apply (inv.all, x.state);
      end return;
   end Apply;


   function decide (
    inv : Invocation)
                   return Operations.Result is

      mine, after : Cell;


      use Cell_Consensus_Protocol;

      procedure decide is new Update_Consensus_Protocol.decide (P => P);

      use Update_Consensus_Protocol;





   begin

      mine := new (My_Cell_Pool.Private_Pool(P)'Access) Cell_Record;
      Reset_Cell (mine);
      mine.inv := inv;

      Announce (P) := mine;

      for Q in Process loop
         Head (P) := (if Head (Q).seq > Head (P).seq then Head (Q)
                    else Head (P));
      end loop;

      for step in 1 .. Consensus_Number + 1 loop
         declare
            help, prefer : Cell;
            head : Cell renames UCO.Head (P);
         begin

            exit when Announce (P).seq /= 0;

            help := Announce (Process'Val(head.seq mod Consensus_Number + Integer(Process'First)));
            prefer := (if help.seq = 0
                then help
                else Announce (P));

            after := decide (head.after, prefer);

            decide (after.update, Apply (after.inv,  head.update.x.state));
            after.before := head;
            after.seq    := head.seq + 1;
         end;

            Head (P) := after;

      end loop;

      Assert (Announce (P) . seq /= 0, "Protocol didn't work.");

      Head (P) := Announce (P);

      declare
         cell : Universal_Consensus_Protocol.Cell := Head (P);
      begin
         for distance in 1 .. Consensus_Number + 1 loop
            cell := cell.before;
            Decrement_Reachability (cell);
         end loop;
      end;

      return Head (P).update.x.result;

   end decide;

--    procedure Finalize (Object : in out Private_Pool_Record)
--     is
--     begin
--        Put_Line ("Finalizando...");
--     end Finalize;


begin
   --Initial_Cell_Record.seq := 1;

  -- Initial_Cell_Record.count := Atomic_Unsigned (Consensus_Number + 1);
 --  Initial_Cell_Record.before := Initial_Cell_Record'Access;

   for i in Pool'Last - (Consensus_Number + 1) + 1
     .. Pool'Last loop
      Reset (Pool (i));
      Set_Before (Pool (i), i - 1);
      Set_Reachability(Pool (i), Consensus_Number + 1 - (Pool'Last - i));
   end loop;

   Pool(Pool'Last) . seq := 1;


   declare
      use Operations;
      procedure decide
      is new Update_Consensus_Protocol.decide (P => Process'First);
      s : State ;
   begin
      decide (Initial_Cell_Record.update,
              Apply(Initialize'Access, s));
   end;

   for p in Process loop
      My_Cell_Pool.Private_Pool (p) . P := p;
    Set_Pool_Of_Subpool
      (My_Cell_Pool.Private_Pool (p)'Unchecked_Access,
       My_Cell_Pool);
   end loop;




end Universal_Consensus_Protocol;
