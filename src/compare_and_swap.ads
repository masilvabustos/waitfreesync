


generic
   type Value is private;
   pragma Compile_Time_Error (Value'Size not in 8 | 16 | 32 | 64,
    "Value'Size must be 8, 16, 32 or 64" & ASCII.LF);
package Compare_And_Swap is


   function Compare_And_Swap (ptr : aliased in out Value;
    expected, desired : Value)
                             return Value;

end Compare_And_Swap;
