
with Ada.Assertions;
use Ada.Assertions;

with Ada.Text_IO; use Ada.Text_IO;

package body Universal_Consensus_Protocol is

   function decide (object : in out Consensus_Object;
    inv : Invocation)
                   return State is

      mine, after : Cell;
      Announce : Cell_Array renames object.Announce;
      Head : Cell_Array renames object.Head;

      use Cell_Consensus_Protocol;

      --function Allocate_Cell return Cell;
      function Allocate_Cell return Cell is
         cell : Universal_Consensus_Protocol.Cell;
         pool : access Cell_Pool := object.Pool (P) 'Access
           ;
      begin
         for C in pool'Range loop
            cell := pool (C) 'Access;
            exit when cell.count = 0;
         end loop;

         Assert (cell.count = 0, "This shouldn't happen (1).");

         return cell;
      end Allocate_Cell;

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

      procedure decide
        (object : aliased in out Update_Consensus_Protocol.Consensus_Object;
         prefer : Operations.State)
      is

         procedure decide is new Update_Consensus_Protocol.decide (P => P);
      begin
        decide (object, prefer);
      end decide;

      use Update_Consensus_Protocol;


   begin

      mine := Allocate_Cell;
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
            head : Cell renames object.Head (P);
         begin

            exit when Announce (P).seq /= 0;

            help := Announce (Process'Val(head.seq mod Consensus_Number));
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
            Decrement (cell.count);
         end loop;
      end;

      return get_value(Head (P).update);

   end decide;




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


end Universal_Consensus_Protocol;
