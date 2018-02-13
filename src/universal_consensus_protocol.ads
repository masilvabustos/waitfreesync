
with CAS_Consensus_Protocol;
with Universal_Consensus_Object_Operations;
with System.Atomic_Counters;

generic
   type Process is range <>;
   with package Operations
     is new Universal_Consensus_Object_Operations (<>);

package Universal_Consensus_Protocol is

   use System.Atomic_Counters;

   Consensus_Number : constant Integer := Process'Pos(Process'Last);
   use Operations;
   
   subtype Private_Pool_Index 
   is Integer range 1 .. Consensus_Number*(Consensus_Number + 1);
   
   type Cell_Record;
   type Cell is access all Cell_Record;
   type Cell_TATA is 
      record
         p : Process;
         i : Private_Pool_Index;
      end record
     with Pack => True;
   
   pragma Compile_Time_Error(Cell'Size > 64, "Oh, oh.");
   
   
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
   type Cell_Pool is array (Process, Private_Pool_Index) of aliased Cell_Record;

   type Consensus_Object is
   limited record
      Announce : Cell_Array := (others => Initial_Cell_Record'Access);
      Head     : Cell_Array := (others => Initial_Cell_Record'Access);
      Pool     : Cell_Pool := (others => 
                                  (others => 
                                     Cell_Record'(
                                       count => 0,
                                      others => <>)));
   end record;

   generic
    P : Process;
   function decide (
    object : aliased in out Consensus_Object;
    inv : Invocation)
                   return State;

end Universal_Consensus_Protocol;
