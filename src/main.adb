
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

  -- type Operation is (Nop, Init, Enqueue, Dequeue);

   type State is record
      Queue : Main.Queue;
      first, last : Queue_elem;
      result : Main.Result;
   end record;

   package Queue_Operations is
     new Universal_Consensus_Object_Operations
       (State => State, Result => Result);

   type Enqueue is new Queue_Operations.Invocation_Base with
      record
         enq_value : Queue_item;
      end record;

   function Apply (inv : Enqueue;
                   s : in out State)
                   return Result
   is
   begin
      if s.result.status = Queue_Full
        or (s.last = s.first and s.result.status = Queue_EnqOk)
      then
         s.result := Main.Result'(status => Queue_Full);
      else
         s.Queue (s.last) := inv.enq_value;
         s.last := s.last + 1;
         s.result := Main.Result'(status => Queue_EnqOk);
      end if;
      return s.result;
   end Apply;

   type Dequeue is new Queue_Operations.Invocation_Base
   with null record;

    function Apply (inv : Dequeue;
                   s : in out State)
                   return Result
   is
   begin
      if s.result.status = Queue_Empty
        or (s.last = s.first and s.result.status = Queue_DeqOk)
      then
         s.result := Main.Result'(status => Queue_Empty);
      else
         declare
            val : constant Queue_item := s.Queue (s.first);
         begin
            s.first  := s.first + 1;
            s.result := Main.Result'(Queue_DeqOk, val);
         end;
      end if;
      return s.result;
   end Apply;


   type Initialize is new Queue_Operations.Invocation_Base
   with null record;

   function Apply (inv : Initialize;
                   s : in out State)
                   return Result
   is
   begin
      s := State'(others => <>);
      return s.result;
   end Apply;








   use Queue_Operations;



   Init : aliased Initialize;

   package Queue_Protocol is new Universal_Consensus_Protocol
        (Process => Main.Process,
         Operations => Queue_Operations,
         Policy => Universal_Consensus_Traits.GENERAL,
        Initialize => Init'Access);

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
      for i in 1 .. 500000 loop
         Put_Line ("Producer " & Integer'Image (Main.Process'Pos (Process)) & ": " & Integer'Image (i));

         loop
            declare
               r : Result;
               function decide is new Queue_Protocol.decide
                 (P => Process);
               inv : Invocation
                 := new Enqueue'(enq_value => Queue_item'(i, Process));
            begin
               r := decide (inv);
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
      for n in 1 .. 500000 loop
         loop
            r := decide (new Dequeue);
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
