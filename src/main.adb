
with Universal_Consensus_Protocol;
with Universal_Consensus_Object_Operations;
with Universal_Consensus_Traits;

with Ada.Text_IO;
with Ada.Assertions;
with Ada.Exceptions;

with System;
use System;
with System.Storage_Elements;

procedure Main is

   type Process is range -1 .. 10;
   Queue_size : constant Positive := 10;
   type Queue_elem is mod Queue_size with Default_Value => 0;

   type Queue_item is record
       value : Integer;
       owner : Process;
   end record;

   type Queue is array (Queue_elem) of Queue_item;

   type Result_Status
   is (Queue_Full, Queue_Empty, Queue_DeqOk, Queue_EnqOk);


   type Result (status : Result_Status := Queue_Empty) is
      record
         case status is
            when Queue_Full =>
               null;
            when Queue_Empty =>
               null;
            when Queue_EnqOk =>
               null;
            when Queue_DeqOk =>
               value : Queue_item;
         end case;
      end record;

   type Result_Wrapper is record
      r : Result;
   end record;

   type Operation is (Nop, Init, Enqueue, Dequeue);

   type State is record
      Queue : Main.Queue;
      first, last : Queue_elem;
      result : Main.Result;
   end record;

   package Queue_Operations is
     new Universal_Consensus_Object_Operations
       (State => State, Result => Result_Wrapper);

   type Queue_Invocation (Operation : Main.Operation) is
   new Queue_Operations.Invocation_Base with

      record
         case Operation is
            when Nop =>
               null;
            when Init =>
               null;
            when Enqueue =>
               enq_value : Queue_item;
            when Dequeue =>
               null;
         end case;
   end record;



   use Queue_Operations;

   function Apply (inv : Queue_Invocation
                    ; prev : in State;
                    s : out State
                    ) return Result_Wrapper
   is

   begin

      case inv.Operation is
         when Nop =>
            null;
         when Init =>
            s := State'(others => <>);
         when Enqueue =>
            if prev.result.status = Queue_Full
              or (prev.last = prev.first and prev.result.status = Queue_EnqOk)
            then
               s := prev;
               s.result := Main.Result'(status => Queue_Full);
            else
               s.Queue := prev.Queue;
               s.Queue (prev.last) := inv.enq_value;
               s.last := prev.last + 1;
               s.first := prev.first;
               s.result := Main.Result'(status => Queue_EnqOk);
            end if;
         when Dequeue =>
            if prev.result.status = Queue_Empty
              or (prev.last = prev.first and prev.result.status = Queue_DeqOk)
            then
               s := prev;
               s.result := Main.Result'(status => Queue_Empty);
            else
               declare
                  val : constant Queue_item := prev.Queue (prev.first);
               begin
                  s.Queue  := prev.Queue;
                  s.last   := prev.last;
                  s.first  := prev.first + 1;
                  s.result := Main.Result'(Queue_DeqOk, val);
               end;
            end if;
      end case;
      return Result_Wrapper'(r => s.result);
   end Apply;

   Initialize : aliased Queue_Invocation := (Operation => Init, others => <>);

   package Queue_Protocol is new Universal_Consensus_Protocol
        (Process => Main.Process,
         Operations => Queue_Operations,
         Policy => Universal_Consensus_Traits.GENERAL,
        Initialize => Initialize'Access);

   use Ada.Text_IO;

   --Consensus_Queue : aliased Queue_Protocol.Consensus_Object;

   task type Producer (Process : Main.Process) is
   end Producer;

   package Process_IO is new Ada.Text_IO.Integer_IO (Num => Process);
   use Process_IO;

   package Integer_IO is new Ada.Text_IO.Integer_IO (Num => Integer);
   use Integer_IO;

   task body Producer is

   begin
      for i in 1 .. 500 loop
         Put_Line ("Producer " & Integer'Image (Main.Process'Pos (Process)) & ": " & Integer'Image (i));

         loop
            declare
               r : Result;
               function decide is new Queue_Protocol.decide
                 (P => Process);
               inv : Invocation
                 := new Queue_Invocation'(Operation => Enqueue,
                                          enq_value => Queue_item'(i, Process));
            begin
               r := decide (inv) . r;
               exit when r.status = Queue_EnqOk;
               delay 10.0e-3;
            end;
         end loop;

      end loop;

      Flush;

      exception
         when a : Ada.Assertions.Assertion_Error =>
            Put_Line (Ada.Exceptions.Exception_Message (a));
      when x : others =>
         Put_Line (Ada.Exceptions.Exception_Information (x));

   end Producer;

   task type Consumer (Process : Main.Process) is
   end Consumer;

   task body Consumer is
      r : Result;

      function decide is new Queue_Protocol.decide
        (P => Process);


   begin
      Put_Line ("Consumer started.");
      for n in 1 .. 500 loop
         loop
            r := decide (new Queue_Invocation'(Operation => Dequeue)) . r;
            exit when r.status = Queue_DeqOk;
            delay 10.0e-3;
         end loop;
         Put_Line ("Consumer " & Integer'Image(Main.Process'Pos(Process)) & ": from"
              & Integer'Image (Main.Process'Pos(r.value.owner))
             & " => " & Integer'Image (r.value.value));

      end loop;

      exception
         when a : Ada.Assertions.Assertion_Error =>
            Put_Line (Ada.Exceptions.Exception_Message (a));
      when x : others =>
         Put_Line (Ada.Exceptions.Exception_Information (x));
   end Consumer;

   cons : Consumer (Process => 1);
   prod : Producer (Process => 2);
   c2 : Consumer (Process => 3);
   p2 : Producer (Process => 4);
   c3 : Consumer (Process => 5);
   p3 : Producer (Process => 6);




begin
   Put_Line ("Hello, world!");
end Main;
