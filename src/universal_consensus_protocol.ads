
with CAS_Consensus_Protocol;
with Universal_Consensus_Object_Operations;
with System.Atomic_Counters;

generic
   type Process is range <>;
   with package Operations
     is new Universal_Consensus_Object_Operations (<>);

package Universal_Consensus_Protocol is

   use System.Atomic_Counters;

   Consensus_Number : constant Standard.Integer
    := Standard.Integer (Process'Last) - Standard.Integer (Process'First) + 1;
   use Operations;
   
   type Cell_Record;
   type Cell is access all Cell_Record;
   
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

   Initial_Cell_Record : aliased Cell_Record;

   type Cell_Array is array (Process) of Cell
     with Volatile => True;
   type Cell_Pool is array (1 .. Consensus_Number**2) of aliased Cell_Record;
   type Pool_Array is array (Process) of aliased Cell_Pool;

   type Consensus_Object is
   limited record
      Announce : Cell_Array := (others => Initial_Cell_Record'Access);
      Head     : Cell_Array := (others => Initial_Cell_Record'Access);
      Pool     : Pool_Array := (others => 
                                  (others => 
                                     Cell_Record'(
                                       count => 0,
                                      others => <>)));
   end record;

   generic
    P : Process;
   function decide (
    object : in out Consensus_Object;
    inv : Invocation)
                   return State;

end Universal_Consensus_Protocol;
