
with Ada.Assertions;
use Ada.Assertions;

with Ada.Text_IO; use Ada.Text_IO;

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
   begin
      case Policy is
         when GENERAL =>
            declare
               N : constant Cell_Record_Pool_Index
                 := Cell_Record_Pool_Index (Consensus_Number);
               L : constant Cell_Record_Pool_Index
                 := N*(N +1);
               S : constant Cell_Record_Pool_Index
                 := Process'Pos (Handle.P) - Process'Pos (Process'First);
               subtype X is
                 Cell_Record_Pool_Index range
                    S * L ..  (S+1) * L - 1;
            begin
               for I in X'Range loop
                  Storage_Address := UCO.Pool(I) 'Address;
                  exit when UCO.Pool(I).count = 0;
               end loop;
               Assert (UCO.Pool(X'Last).count = 0, "Did not find cell. This shouldn't happen (1).");
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


   function decide (
    inv : Invocation)
                   return State is

      mine, after : Cell;


      use Cell_Consensus_Protocol;

      procedure Reset_Cell (cell : in Universal_Consensus_Protocol.Cell);
      procedure Reset_Cell (cell : in Universal_Consensus_Protocol.Cell)
      is
         use Update_Consensus_Protocol;
      begin
         reset (cell.after);
         reset (cell.update);
         cell.count := Atomic_Unsigned (Consensus_Number + 1);
         cell.seq := 0;
      end Reset_Cell;

      procedure Decrement_Reachability (cell : in UCO.Cell)
      is
      begin
         Decrement (cell.count);
      end Decrement_Reachability;


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

            decide (after.update, Apply (after.inv, get_value(head.update)));
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

      return get_value(Head (P).update);

   end decide;

--    procedure Finalize (Object : in out Private_Pool_Record)
--     is
--     begin
--        Put_Line ("Finalizando...");
--     end Finalize;


begin
   Initial_Cell_Record.seq := 1;

   Initial_Cell_Record.count := Atomic_Unsigned (Consensus_Number + 1);
   Initial_Cell_Record.before := Initial_Cell_Record'Access;

   declare
      use Operations;
      procedure decide
      is new Update_Consensus_Protocol.decide (P => Process'First);
   begin
      decide (Initial_Cell_Record.update,
              Apply(Initialization,
                Initial_Cell_Record.update.prefer (Process'First)));
   end;

   for p in Process loop
      My_Cell_Pool.Private_Pool (p) . P := p;
    Set_Pool_Of_Subpool
      (My_Cell_Pool.Private_Pool (p)'Unchecked_Access,
       My_Cell_Pool);
   end loop;




end Universal_Consensus_Protocol;
