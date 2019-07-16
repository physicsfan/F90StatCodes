!######################################################################
module randomN
      ! a simple module for generating randum numbers
contains
	  subroutine init_random_seed()
			integer :: i, n, clock
			integer, dimension(:), allocatable :: seed

			call random_seed(size = n)
			allocate(seed(n))

			call system_clock(count=clock)

			seed = clock + 37 * (/ (i - 1, i = 1, n) /)
			call random_seed(PUT = seed)

			deallocate(seed)
	  end subroutine init_random_seed
      
	  subroutine fill_with_uniforms(vec, lower, upper)
            ! Fills the rank_one array vec with random numbers uniformly
            ! distributed from lower to upper, which default to 0.0 and
            ! 1.0, respectively
            implicit none
            ! declare arguments
            real, intent(out) :: vec(:)
            real, intent(in), optional :: lower, upper
            ! declare locals
            real :: a, b
            ! begin
            a = 0.0
            b = 1.0
            if(present(lower)) a = lower
            if(present(upper)) b = upper
            call random_number(vec)
            vec = a + vec * (b - a)
      end subroutine fill_with_uniforms
end module randomN
!######################################################################
program uniform
  ! Generates random numbers uniformly distributed between a and b
  use randomN      			! allows the program to use fill_with_uniforms
  implicit none

  ! declare variables
  integer :: i, n
  real :: a, b
  real, allocatable :: x(:)
  call init_random_seed()	! initialize random number generator

  
  print "(A)", "Enter the lower an upper bounds:"
  read(*,*) a, b
  print "(A)", "How many random numbers do you want?"
  read(*,*) n
  allocate(x(n))
  
  call fill_with_uniforms(x, upper=b, lower=a)
  
  print "(A)", "Here they are:"
  do i = 1, n
      print *, x(i)
  end do
  deallocate(x)   ! not really necessary

end program uniform
