
with CAS_Consensus_Protocol;

generic
    type Invocation is private;
    type State is private;
    type Result is private;
    with function Apply (inv : Invocation; s : in out State)
        return Result;
package Universal_Consensus_Object_Operations is

   type Update is
      record
         state : Universal_Consensus_Object_Operations.State;
         result : Universal_Consensus_Object_Operations.Result;
      end record;

   Initial_Update : aliased Update;
   type Update_Access is access all Update;

   package Update_Consensus_Protocol is
     new CAS_Consensus_Protocol (Value => Update_Access,
        undecided => null);

   subtype Consensus_Object is Update_Consensus_Protocol.Consensus_Object;


   function Apply (inv : Invocation; current : Consensus_Object) return Update;
   procedure decide (object : in out Consensus_Object; prefer : Update);
   procedure reset (object : in out Consensus_Object);

end Universal_Consensus_Object_Operations;
