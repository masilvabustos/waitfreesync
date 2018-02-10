
with CAS_Consensus_Protocol;

generic
   type Invocation is private;
   type State is private;
   with procedure Apply (inv : Invocation
                         ; s : in out State);
   Initialization : Invocation;
package Universal_Consensus_Object_Operations is

   package This renames Universal_Consensus_Object_Operations;

   type Update is private;


   function Apply (inv : Invocation; s : State)
                   return State;

private

   type Update is new State;





end Universal_Consensus_Object_Operations;
