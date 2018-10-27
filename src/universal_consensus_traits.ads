package Universal_Consensus_Traits is

   type Policy is (GENERAL, LIFO);
   type Cell_Size is (Size_24, Size_Full);
   for Cell_Size use (24, 2**32-1);


end Universal_Consensus_Traits;
