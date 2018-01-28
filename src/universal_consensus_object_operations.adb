
with Ada.Unchecked_Deallocation;

package body Universal_Consensus_Object_Operations is

   function Apply (inv : Invocation; current : Consensus_Object)
    return Update is
         ud : Update;
   begin
         ud.state := current.value.state;
         ud.result
           := Apply (inv, ud.state);
         return ud;
   end Apply;

   procedure decide
        (object : in out Consensus_Object;
         prefer : Update) is

      use Update_Consensus_Protocol;

      val : Update_Access;
      new_prefer : Update_Access := new Update;

         procedure Free is new
           Ada.Unchecked_Deallocation
            (Object => Update, Name => Update_Access);
   begin
      new_prefer.all := prefer;
         val := decide (object, new_prefer);
         if val /= new_prefer then
            Free (new_prefer);
         end if;
   end decide;

   procedure reset (object : in out Consensus_Object) is
      procedure Free is new
         Ada.Unchecked_Deallocation
          (Object => Update, Name => Update_Access);
   begin
      Free (object.value);
   end reset;

end Universal_Consensus_Object_Operations;
