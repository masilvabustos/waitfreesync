
with Ada.Assertions;
use Ada.Assertions;

with Ada.Text_IO; use Ada.Text_IO;

package body Universal_Consensus_Protocol is

   function decide (object : in out Consensus_Object;
    inv : Invocation)
                   return Result is

      mine, after : Cell;
      Announce : Cell_Array renames object.Announce;
      Head : Cell_Array renames object.Head;

      use Cell_Consensus_Protocol;

      function Allocate_Cell (pool : aliased in out Cell_Pool) return Cell;
      function Allocate_Cell (pool : aliased in out Cell_Pool) return Cell is
         cell : Universal_Consensus_Protocol.Cell;
      begin
         for C in pool'Range loop
            cell := pool (C) 'Access;
            exit when cell.count = 0;
         end loop;

         Assert (cell.count = 0, "This shouldn't happen (1).");

         for C in pool'Range loop
            if
                pool (C) . count = 0
            then
               cell := (if pool (C) . seq . count < cell.seq.count
                    then pool (C)'Access
                    else cell);
            end if;
         end loop;

         Assert (cell.count = 0, "This shouldn't happen (2).");

         return cell;
      end Allocate_Cell;

      procedure Reset_Cell (cell : in Universal_Consensus_Protocol.Cell);
      procedure Reset_Cell (cell : in Universal_Consensus_Protocol.Cell) is
         use Operations.Update_Consensus_Protocol;
      begin
         reset (cell.after);
         Operations.reset (cell.update);
         cell.count := Atomic_Unsigned (Consensus_Number + 1);
         cell.seq.count := 0;
      end Reset_Cell;

   begin

      mine := Allocate_Cell (pool.all);
      Reset_Cell (mine);
      mine.inv := inv;

      Announce (P) := mine;

      for Q in Process loop
         Head (P) := (if Head (Q).seq.count > Head (P).seq.count then Head (Q)
                    else Head (P));
      end loop;

      for step in 1 .. Consensus_Number + 1 loop
         declare
            help, prefer : Cell;
            head : Cell renames object.Head (P);
         begin

            exit when Announce (P).seq.count /= 0;

            help := Announce (head.seq.next);
            prefer := (if help.seq.count = 0
                then help
                else Announce (P));

            after := decide (head.after, prefer);

            Operations.decide (after.update, Apply (after.inv, head.update));
            after.before := head;
            after.seq    := Sequence'(head.seq.count + 1, head.seq.next + 1);
         end;

            Head (P) := after;

      end loop;

      Assert (Announce (P) . seq . count /= 0, "Protocol didn't work.");

      Head (P) := Announce (P);

      declare
         cell : Universal_Consensus_Protocol.Cell := Head (P);
      begin
         for distance in 1 .. Consensus_Number + 1 loop
            cell := cell.before;
            Decrement (cell.count);

         end loop;
         exception
         when Constraint_Error =>
            Put_Line ("constraint error");
            raise;
      end;

      return Head (P).update.value.result;

   end decide;

begin
   Initial_Cell_Record.seq := Sequence'(1, Process'First);
   Initial_Cell_Record.update.value := Operations.Initial_Update'Access;
   Initial_Cell_Record.count := Atomic_Unsigned (Consensus_Number + 1);
   Initial_Cell_Record.before := Initial_Cell_Record'Access;

end Universal_Consensus_Protocol;
