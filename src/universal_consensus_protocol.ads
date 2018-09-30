
with CAS_Consensus_Protocol;
with Universal_Consensus_Object_Operations;
with System.Atomic_Counters;
with System.Storage_pools;
with System.Storage_Pools.Subpools;
use System.Storage_Pools.Subpools;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Universal_Consensus_Traits; use Universal_Consensus_Traits;

with Ada.Finalization;

generic
  type Process is range <>;
  -- type Cell_Reference is mod <>;
   with package Operations
     is new Universal_Consensus_Object_Operations (<>);
   --Consensus_Number : Positive;
   Policy : Universal_Consensus_Traits.Policy;
   
package Universal_Consensus_Protocol is
   
   package UCO renames Universal_Consensus_Protocol;

   use System.Atomic_Counters;
   use System.Storage_Pools;

   use Operations;
   
   Consensus_Number : constant Positive := Process'Range_Length;
   --type Process is new Positive range 1 .. Consensus_Number;
   

   
   type Private_Pool_Record is new Subpools.Root_Subpool with
      record
         P : Process;
      end record;

   --type Invocation is tagged null record;
   type Private_Pool_Array is array (Process) of aliased Private_Pool_Record;
   
   
   
   
   type Cell_Pool is 
     new Root_Storage_Pool_With_Subpools with 
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

   type Cell_Record;
   type Cell is access all Cell_Record;
   for Cell'Storage_Pool use My_Cell_Pool;
   
   package Cell_Consensus_Protocol 
   is new CAS_Consensus_Protocol.Access_Consensus_Protocol 
     (Designated => Cell_Record, Value => Cell);
   
   package Update_Consensus_Protocol
   is new CAS_Consensus_Protocol.Record_Consensus_Protocol
     (Value => Operations.State, Process => Process);
     

   type Cell_Record is record
      seq    : Natural := 0;
      after  : Cell_Consensus_Protocol.Consensus_Object;
      inv    : Invocation;
      update : aliased Update_Consensus_Protocol.Consensus_Object;
      --  GC information
      count  : aliased Atomic_Unsigned := 0;
      before : Cell;
   end record;
   
   protected type Compact_Cell_Fields
     with Lock_Free => True
   is
      procedure Decide_After(prefer : in out Integer);
      procedure Decrement_Count;
      function Count return Natural;
   private
      register : Natural := 0;
   end Compact_Cell_Fields;
   
   type Compact_Cell_Record is record
         seq : Natural := 0;
         fields : Compact_Cell_Fields;
      end record;
  

   Initial_Cell_Record : aliased Cell_Record;

   type Cell_Array is array (Process) of Cell
     with Volatile => True;

   type Cell_Record_Pool_Index is 
     new Natural range 
       0 .. (case Policy is 
                when GENERAL => Consensus_Number**2*(Consensus_Number - 1),
                when LIFO => Consensus_Number*(Consensus_Number - 1)/2);
   
   type Cell_Record_Pool is array (Cell_Record_Pool_Index) of aliased Cell_Record;


   generic
      P : Process;
   function decide (inv : Invocation)
                    return State;
   
private
   
   Announce : Cell_Array := (others => Initial_Cell_Record'Access);
   Head     : Cell_Array := (others => Initial_Cell_Record'Access);
   
   Pool : aliased Cell_Record_Pool
     := (others => Cell_Record'(count => 0, others => <>)); 

end Universal_Consensus_Protocol;
