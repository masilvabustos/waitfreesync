
with System;
with Ada.Unchecked_Conversion;

package CAS_Consensus_Protocol is

   generic
      type Value is range <>;

   package Compare_And_Swap is

      subtype Formal_Value is CAS_Consensus_Protocol.Compare_And_Swap.Value;

      protected type Object
        with Lock_Free => True
      is
         procedure compare_and_swap (cmp : Formal_Value; prefer : in out Formal_Value);
         procedure set (x : Formal_Value);
         function get return Formal_Value;
      private
         value : Formal_Value;
      end Object;
   end Compare_And_Swap;

   generic
      type Value is range <>;
   package Integer_Consensus_Protocol is

      protected type Consensus_Object (bottom : Value)
        with Lock_Free => True
      is
         procedure decide (prefer : in out Value);
         function get return Value;
         procedure reset;
      private
         value : Integer_Consensus_Protocol.Value;
      end;

      function decide (object : in out Consensus_Object; prefer : Value)
                       return Value;

   end Integer_Consensus_Protocol;



   generic
      type Designated;
      type Value is access all Designated;
   package Access_Consensus_Protocol is

      package Consensus is new Compare_And_Swap (Value => Long_Integer);

     -- subtype Consensus_Object is CAS.Object;

      function decide (object : in out Consensus.Object; prefer : access Designated)
                       return Value;
      function get_value (object : Consensus.Object)
                          return Value;

      procedure reset (object : in out Consensus.Object);

   end Access_Consensus_Protocol;

   generic
      type Value is private;
      type Process is range <>;
   package Record_Consensus_Protocol is

      type Access_Value is access all Value;

      package Decision
      is new Access_Consensus_Protocol
        (Designated => Value, Value => Access_Value);

      function To_Long_Integer is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Long_Integer);

      type Prefer_Array is array (Process) of aliased Value;

      type Consensus_Object
      is limited record
         r : Decision.Consensus.Object ;
         prefer : Prefer_Array;
      end record;

      generic
         P : Process;
      procedure decide (object : aliased in out Consensus_Object;
                        prefer : Value);


      function get_value (object : Consensus_Object)
                      return Record_Consensus_Protocol.Value;

      procedure reset (object : in out Consensus_Object);

   end Record_Consensus_Protocol;





end CAS_Consensus_Protocol;
