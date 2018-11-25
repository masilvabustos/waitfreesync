
with CAS_Consensus_Protocol;
with Universal_Consensus_Object_Operations;
with System.Atomic_Counters;
with System.Storage_pools;
with System.Storage_Pools.Subpools;
use System.Storage_Pools.Subpools;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Universal_Consensus_Traits; use Universal_Consensus_Traits;
with Universal_Consensus_Elements;

with Ada.Finalization;
with Ada.Unchecked_Conversion;

generic
  type Process is range <>;
  -- type Cell_Reference is mod <>;
   with package Operations
     is new Universal_Consensus_Object_Operations (<>);
   --Consensus_Number : Positive;
  -- type Invocation is access Operations.Invocation'Class;
   
   Initialize : Operations.Invocation'Class;
   
   Policy : Universal_Consensus_Traits.Policy;
   
package Universal_Consensus_Protocol is
   
   package UCO renames Universal_Consensus_Protocol;
   
   package Elements is new Universal_Consensus_Elements (Process    => Process,
                                                         Operations => Operations,
                                                         Initialize => Initialize,
                                                         Policy     => Policy);
   
   use Elements;
   
   subtype Invocation is Elements.Invocation;

   use System.Atomic_Counters;
   
   

   Consensus_Number : constant Positive := Process'Range_Length;
    
   
   
  
   generic
      P : Process;
   function decide (inv : Invocation)
                    return Operations.Result;
   
private
   
   type Cell_Array is array (Process) of Cell
     with Volatile => True;
   
   
   Announce : Cell_Array;
   Head     : Cell_Array;
   

   


end Universal_Consensus_Protocol;
