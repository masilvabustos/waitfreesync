
with Ada.Assertions;
use Ada.Assertions;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Universal_Consensus_Protocol is




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

   procedure Reset_Cell (cell : in Elements.Cell)
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

   procedure Decrement_Reachability (cell : in Elements.Cell)
   is
   begin
      Decrement_Reachability (cell.all);
   end Decrement_Reachability;

   procedure Set_Before(cell : in out Cell_Record;
                        before : Elements.Cell)
   is
      pragma Compile_Time_Warning (true, "Set Before not fully implemented.");
   begin
      cell.before := before;
   end Set_Before;



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

      mine := new Cell_Record;
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
         cell : Elements.Cell := Head (P);
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

   declare
      Initial_Cell : Cell := new Cell_Record'(seq    => 1,
                                    after  => <>,
                                    inv    => <>,
                                    update => <>,
                                    count  => Atomic_Unsigned (Consensus_Number + 1),
                                    before => <>);


      cell : Elements.Cell := Initial_Cell;

      use Operations;
      procedure decide
      is new Update_Consensus_Protocol.decide (P => Process'First);
      s : State ;

   begin

      for c in reverse 1 .. (Consensus_Number + 1) - 1 loop
         cell.before := new Cell_Record'(seq => 1,
                                         after  => <>,
                                         inv    => <>,
                                         update => <>,
                                         count  => Atomic_Unsigned (c),
                                         before => <> );
         cell := cell.before;
      end loop;

      Announce := (others => Initial_Cell);
      Head     := (others => Initial_Cell);

      decide (Initial_Cell.update,
              Apply(Initialize'Access, s));

   end;

end Universal_Consensus_Protocol;
