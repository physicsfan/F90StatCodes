program temperature_conversion
      ! Converts either F to C or C to F as specified by the user
      implicit none

      integer :: scale_type
      real :: initial_temp

      ! Print headings
      print "(A)", "Temperature Conversion Program"
      print "(A)", "------------------------------"
      print *, ""

      ! Select starting temperature scale
      print "(A)", "Enter the starting temperature scale: (C=1 or F=2)"
      read(*,*) scale_type
      print "(A)", "Enter initial temperature"
      read(*,*) initial_temp

      ! Print the final converted temperature
      if(scale_type == 1) then
            print *, ""
            print "(A)", "The temperature in F is:"
            print *, 9.*initial_temp/5. + 32.
      else
            print *, ""
            print "(A)", "The temperature in C is:"
            print *, 5.*(initial_temp-32.)/9.
      end if

end program
