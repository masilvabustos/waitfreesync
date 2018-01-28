generic

   type Value is private;
   undecided : Value;

package CAS_Consensus_Protocol is

   type Consensus_Object is
   limited record
      value : aliased CAS_Consensus_Protocol.Value := undecided;
   end record with Volatile => True;


   function decide (object : in out Consensus_Object; desired : Value)
     return Value;

   procedure reset (object : in out Consensus_Object);


end CAS_Consensus_Protocol;
