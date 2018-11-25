
with System;
use System;

with System.Storage_Pools;
use  System.Storage_Pools;
with System.Storage_Pools.Subpools;
use System.Storage_Pools.Subpools;

with Universal_Consensus_Traits;
with System.Storage_Elements; use System.Storage_Elements;

generic
   
   Policy : Universal_Consensus_Traits.Policy;
   
   type Process is range <>;

package Cell_Pool is
   
   use System.Storage_Pools;
   
   Consensus_Number : Positive := Process'Range_Length;

  type Private_Pool_Record is new Subpools.Root_Subpool with
      record
         P : Process;
      end record;


   type Private_Pool_Array is array (Process) of aliased Private_Pool_Record;
   
   type Cell_Pool is 
     new Subpools.Root_Storage_Pool_With_Subpools with 
      record
         Private_Pool : Private_Pool_Array;
      end record;
   
   --procedure Finalize (Object : in out Private_Pool_Record) is null;
   
   
   type Private_Pool_Handle is access all Private_Pool_Record;
   
   procedure Allocate_From_Subpool 
     (Pool : in out Cell_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : in Storage_Elements.Storage_Count;
      Alignment : in Storage_Elements.Storage_Count;
      Subpool : in not null Subpool_Handle);
   
   function Create_Subpool 
     (Pool : in out Cell_Pool)
      return not null Subpool_Handle;
  
   
   procedure Deallocate_Subpool 
     (Pool : in out Cell_Pool;
      Subpool : in out Subpool_Handle) is null;
   
   
   My_Cell_Pool : aliased Cell_Pool; 
   
   
  

   subtype Cell_Record_Pool_Index is 
      Natural range 
       1 .. (case Policy is 
               when Universal_Consensus_Traits.GENERAL => 
                 Consensus_Number*(Consensus_Number*(Consensus_Number + 1) + 1),
                when Universal_Consensus_Traits.PRIORIZED => 
                  Consensus_Number*(Consensus_Number - 1)/2);
   
   type Cell_Record_Pool is array (Cell_Record_Pool_Index) of aliased Cell_Record;

end Cell_Pool;
