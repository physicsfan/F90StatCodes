program pick_random
  ! draws a random sample of size n without replacement from a population {1,2,...,N}
  ! for any N >= 1 and n <= N.
  implicit none
  
  integer :: i, j, sample_size, population_size
  real :: r
  integer, allocatable :: population(:), sample(:)
  logical, allocatable :: drawn(:)    ! records which elements of the population have been drawn
  
  print "(A)", "Enter the upper bound of the population:"
  read(*,*) population_size
  print "(A)", "Size of random sample wanted?"
  read(*,*) sample_size
  
  allocate(population(population_size))
  allocate(sample(sample_size))
  allocate(drawn(population_size))
  
  ! initially no elements of the population are seen
  drawn = .false.
  
  ! initialize the population {1,2,...,N}
  population = (/(i, i = 1, population_size)/)
  
  ! initialize the sample
  sample = 0
  
  ! initialize random number generator
  call init_random_seed()
  
  ! generate sample
  do i = 1, sample_size
     do
         ! select an element from the population
         call random_number(r)
         j = int(r*population_size) + 1
         ! check if element has been seen already
         if(.not.drawn(j)) then
            ! if not seen place population element into the sample
            ! if seen draw again
            sample(i) = population(j)
             drawn(j) = .true.
             exit
          end if
      end do
   end do
  
  ! print the sample
  print "(A)", "Here is the sample:"
  do i = 1, sample_size
     print *, sample(i)
   end do
  
  ! deallocate the arrays used (not really necessary)
  deallocate(drawn, population, sample)

contains

  ! Any functions or subroutines go here...
  
  subroutine init_random_seed()
    ! initializes the random number generator
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
    
    call random_seed(size = n)
    allocate(seed(n))
    
    call system_clock(COUNT=clock)
    
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(PUT = seed)
    
    deallocate(seed)
  end subroutine init_random_seed
  


end program pick_random


