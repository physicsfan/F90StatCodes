program birthday
      ! Calculates the probability for n individuals, that 2 share the same
      ! birthday
      implicit none

      integer :: i, j
      integer, parameter :: bit32=4, bit64=8, bit128=16     ! specify precision
      integer, parameter :: number_of_individuals = 100
      real(kind=bit32) :: different_birthday, same_birthday

      !print "(A)", "         Number     Different                   Same"

      do i = 2, number_of_individuals
            different_birthday = 1.0
            do j = 1, i
                  different_birthday = different_birthday * (365.-j+1)/365.
            end do
            same_birthday = 1.0 - different_birthday
            print *, i, "    ", different_birthday, "     ", same_birthday
      end do

end program
