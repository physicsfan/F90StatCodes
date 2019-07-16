program test_random_number
  ! a short program to test the initialization of the random seed for the
  ! subroutine random_number
  real :: r(2,2)
  call init_random_seed()         ! see example of RANDOM_SEED
  call random_number(r)
  print *, r
  
contains
  
  subroutine init_random_seed()
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
    
    call random_seed(size = n)
    allocate(seed(n))
    
    call system_clock(COUNT=clock)
    
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(PUT = seed)
    
    deallocate(seed)
  end subroutine init_random_seed
  
end program test_random_number
