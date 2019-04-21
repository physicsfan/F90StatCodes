program exponential
      ! Generates random numbers from an exponential distribution a*exp(-a*y)
      ! using the inverse-cdf method y=F^-1(u), u~U(0,1), where F is the cdf.
      implicit none
      integer :: i, n
      real :: u
      real, parameter :: a = 1.5                ! a cannot be 0.0

      print "(A)", "How many do you want?"
      read(*,*) n
      print "(A)", "Here they are:"

      do i = 1, n
            call random_number(u)
            print *, (-1./a)*log(1.-u)
      end do

end program exponential
