
with Compare_And_Swap;

package body CAS_Consensus_Protocol is

   function decide (object : in out Consensus_Object; desired : Value)
                   return Value is
      prev : Value;
      package CAS is new Compare_And_Swap (Value => Value);
   begin
      prev := CAS.Compare_And_Swap (object.value, undecided, desired);
      return (if prev = undecided then desired else prev);
   end decide;

   procedure reset (object : in out Consensus_Object) is
   begin
      object.value := undecided;
   end reset;

end CAS_Consensus_Protocol;
