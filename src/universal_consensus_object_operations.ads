
with CAS_Consensus_Protocol;

generic
   type State is private;
   type Result is private;
package Universal_Consensus_Object_Operations is

   type Invocation_Base is abstract tagged null record;
   type Invocation is access all Invocation_Base'Class;

   function Apply (inv : Invocation_Base;
                   prev : in State;
                   curr : out State)
                   return Result is abstract;

end Universal_Consensus_Object_Operations;
