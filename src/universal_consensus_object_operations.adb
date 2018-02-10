
with Ada.Unchecked_Deallocation;

package body Universal_Consensus_Object_Operations is



   function Apply (inv : Invocation; s : State)
                    return State is
      r : State := s;
   begin
      Apply (inv, r);
      return r;
   end Apply;

end Universal_Consensus_Object_Operations;
