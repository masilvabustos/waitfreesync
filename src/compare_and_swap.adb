
with System.Atomic_Primitives;
with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body Compare_And_Swap is

   use System.Atomic_Primitives;



   function Compare_And_Swap (ptr : aliased in out Value;
    expected, desired : Value)
                             return Value is

      package Address_To_Access_Conversions
      is new System.Address_To_Access_Conversions (Object => Value);

      use Address_To_Access_Conversions;

      address : constant System.Address := To_Address (ptr'Access);

   begin

      case Value'Size is
         when 64 =>
         declare
            function To_uint64
              is new Ada.Unchecked_Conversion
                 (Source => Value, Target => uint64);

            function To_Value
              is new Ada.Unchecked_Conversion
                 (Source => uint64, Target => Value);

            val : uint64;
         begin
            val := Sync_Compare_And_Swap_64
               (address,
                To_uint64 (expected),
                To_uint64 (desired));

            return To_Value (val);
         end;

         when 32 =>
         declare
            function To_uint32
             is new Ada.Unchecked_Conversion
                (Source => Value, Target => uint32);

            function To_Value
             is new Ada.Unchecked_Conversion
                (Source => uint32, Target => Value);

            val : uint32;
         begin
            val := Sync_Compare_And_Swap_32
              (address,
               To_uint32 (expected),
               To_uint32 (desired));

            return To_Value (val);
         end;

         when 16 =>
         declare
            function To_uint16
             is new Ada.Unchecked_Conversion
                (Source => Value, Target => uint16);

            function To_Value
             is new Ada.Unchecked_Conversion
                (Source => uint16, Target => Value);

            val : uint16;
         begin
            val := Sync_Compare_And_Swap_16
              (address,
               To_uint16 (expected),
               To_uint16 (desired));

            return To_Value (val);
         end;

         when others =>
            raise Program_Error;

      end case;

   end Compare_And_Swap;
end Compare_And_Swap;
