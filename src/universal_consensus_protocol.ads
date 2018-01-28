
with CAS_Consensus_Protocol;
with Universal_Consensus_Object_Operations;
with System.Atomic_Counters;

generic
   type Process is mod <>;
   with package Operations
    is new Universal_Consensus_Object_Operations (<>);

package Universal_Consensus_Protocol is

   use System.Atomic_Counters;

   Consensus_Number : constant Standard.Integer
    := Standard.Integer (Process'Last) - Standard.Integer (Process'First) + 1;
   use Operations;



   type Cell_Record;
   type Cell is access all Cell_Record with Atomic => True, Volatile => True;

   package Cell_Consensus_Protocol is
     new CAS_Consensus_Protocol (Value => Cell, undecided => null);

   type Sequence is
       record
        count : Natural := 0;
        next : Process := Process'First;
   end record;

   type Cell_Record is record
      seq    : Sequence;
      after  : aliased Cell_Consensus_Protocol.Consensus_Object;
      inv    : Invocation;
      update : Operations.Consensus_Object;
      --  GC information
      count  : aliased Atomic_Unsigned := 0;
      before : Cell;
   end record;

   Initial_Cell_Record : aliased Cell_Record;

   type Cell_Array is array (Process) of Cell;
   type Cell_Pool is array (1 .. Consensus_Number**2) of aliased Cell_Record;

   type Consensus_Object is
      limited record
         Announce : Cell_Array := (others => Initial_Cell_Record'Access);
         Head     : Cell_Array := (others => Initial_Cell_Record'Access);
      end record with
         Volatile => True;

   generic
    P : Process;
    pool : access Cell_Pool;
   function decide (
    object : in out Consensus_Object;
    inv : Invocation)
                   return Result;

end Universal_Consensus_Protocol;
