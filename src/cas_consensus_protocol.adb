
with System.Address_To_Access_Conversions;

package body CAS_Consensus_Protocol is

   package body Compare_And_Swap
   is
      protected body Object
        is

         procedure compare_and_swap (cmp : Formal_Value; prefer : in out Formal_Value)
         is
         begin
            if
              value = cmp
            then
               value := prefer;
            else
               prefer := value;
            end if;
         end compare_and_swap;

         function get return Formal_Value is
         begin
            return value;
         end get;

         procedure set (x : Formal_Value) is
         begin
            value := x;
         end set;

      end Object;
   end Compare_And_Swap;



   package body Integer_Consensus_Protocol is

      protected body Consensus_Object is

         procedure decide (prefer : in out Integer_Consensus_Protocol.Value) is
         begin
            if
              value = bottom
            then
               value := prefer;
            else
               prefer := value;
            end if;
         end decide;

         function get return Integer_Consensus_Protocol.Value is
         begin
            return value;
         end get;

         procedure reset
         is
         begin
            value := bottom;
         end reset;


      end;

      function decide (object : in out Consensus_Object; prefer : Value)
                       return Value is
         val : Value := prefer;
      begin
         object.decide(val);
         return val;
      end decide;

   end Integer_Consensus_Protocol;


   -- Access Consensus Protocol --

   package body Access_Consensus_Protocol is

      function To_Long_Integer
      is new Ada.Unchecked_Conversion (Source => Value, Target => Long_Integer);

      function To_Value
      is new Ada.Unchecked_Conversion (Source => Long_Integer, Target => Value);

      bottom : constant Long_Integer := To_Long_Integer(Value'(null));

      function decide (object : in out Consensus.Object; prefer : Value)
                       return Value is

         val : Long_Integer := To_Long_Integer(prefer);
      begin
         object.compare_and_swap(bottom, val);
         return To_Value(val);
      end decide;

      function get_value (object : Consensus.Object)
                          return Value
      is
      begin
         return To_Value(object.get);
      end get_value;


      procedure reset (object : in out Consensus.Object)
      is
      begin
         object.set(bottom);
      end reset;


   end Access_Consensus_Protocol;

   package body Record_Consensus_Protocol is

      procedure decide (object : aliased in out Consensus_Object; prefer : Value)
      is
         val : Value renames object.prefer (P);
         y : Access Value := val'Access;
         use Decision;
         Data : Access_Value;
      begin
         val := prefer;
         Data := decide (object.r, y);
      end decide;

      function get_value (object : Consensus_Object )
                          return Value
      is
         use Decision;
      begin
         return get_value(object.r).all;
      end get_value;


      procedure reset (object : in out Consensus_Object)
      is
         use Decision;
      begin
         reset (object.r);
      end reset;

   end Record_Consensus_Protocol;



end CAS_Consensus_Protocol;
