--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body SPDX.Exceptions is

   --------------
   -- Valid_Id --
   --------------

   function Valid_Id (Str : String) return Boolean is
   begin
      return (for some I in Id => Str = Img (I));
   end Valid_Id;

   -------------
   -- From_Id --
   -------------

   function From_Id (Str : String) return Id is
   begin
      for I in Id loop
         if Str = Img (I) then
            return I;
         end if;
      end loop;
      raise Program_Error;
   end From_Id;

end SPDX.Exceptions;
