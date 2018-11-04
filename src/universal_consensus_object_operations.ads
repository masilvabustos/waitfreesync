
with CAS_Consensus_Protocol;

generic
   type State is private;
   type Result is private;
package Universal_Consensus_Object_Operations is

   type Invocation is abstract tagged null record;

   function Apply (inv : Invocation;
                   s : in out State)
                   return Result is abstract;

end Universal_Consensus_Object_Operations;
