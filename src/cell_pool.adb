
package body Cell_Pool is

    procedure Allocate_From_Subpool (
                                   Pool : in out Cell_Pool;
                                   Storage_Address : out Address;
                                   Size_In_Storage_Elements : in Storage_Elements.Storage_Count;
                                   Alignment : in Storage_Elements.Storage_Count;
                                   Subpool : in not null Subpool_Handle)
   is
      Handle : Private_Pool_Handle := Private_Pool_Handle (Subpool);
      Cell : UCO.Cell;
   begin
      case Policy is
         when GENERAL =>
            declare
               N : Cell_Record_Pool_Index
                 renames Consensus_Number;
               L : constant Cell_Record_Pool_Index
                 := N*(N +1);
               S : constant Integer
                 :=
                   + Process'Pos (Handle.P)
                 - Process'Pos (Process'First);
               subtype X is
                 Cell_Record_Pool_Index range
                   Cell_Record_Pool_Index'First + S * L
                     ..  Cell_Record_Pool_Index'First + (S+1) * L ;
            begin

               for I in X loop
                  Cell := UCO.Pool(I) 'Access;
                  exit when Cell.count = 0;
               end loop;
               Assert (Cell.count = 0, "Did not find cell. This shouldn't happen (1).");
               Storage_Address := Cell.all'Address;
            end;
         when LIFO =>
            null;
      end case;
   end Allocate_From_Subpool;

   function Pool_of_Subpool
     (Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_With_Subpools'Class
   is
   begin
      return UCO.My_Cell_Pool'Access;
   end Pool_of_Subpool;


   function Create_Subpool (Pool : in out Cell_Pool)
                              return not null Subpool_Handle
   is
   begin

      raise Storage_Error; --All subpools are created statically.
      return Default_Subpool_For_Pool (Pool);

   end Create_Subpool;

    Pool : aliased Cell_Record_Pool
     := (others => Cell_Record'(count => 0, inv => Initialize'Access, others => <>));

   Initial_Cell_Record : Cell_Record renames Pool (Pool'Last);

end Cell_Pool;
