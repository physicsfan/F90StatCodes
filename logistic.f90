module logistic

contains
  
  subroutine init_random_seed()
    ! Uses the computer clock to provide a different seed
    ! for every call of random_number(). Thus there will be
    ! no repetitions of random numbers.      
    implicit none
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
    call random_seed(size = n)
    allocate(seed(n))
    call system_clock(count=clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(PUT = seed)
    deallocate(seed)
  end subroutine init_random_seed
  
  
  subroutine fill_with_uniforms(mat, lower, upper)
    ! Fills the rank_two array mat with random numbers uniformly
    ! distributed from lower to upper, which default to 0.0 and
    ! 1.0, respectively
    implicit none
    ! declare arguments
    real, intent(out) :: mat(:,:)
    real, intent(in), optional :: lower, upper
    ! declare locals
    real :: a, b
    ! begin
    a = 0.0
    b = 1.0
    if(present(lower)) a = lower
    if(present(upper)) b = upper
    call random_number(mat)
    mat = a + mat * (b - a)
  end subroutine fill_with_uniforms
  
  
  elemental real function expit(x) result(answer)
    implicit none
    real, intent(in) :: x
    
    answer = exp(x)/(1.0+exp(x))
    
  end function expit
  
  
  elemental real function logit(x) result(answer)
    implicit none
    real, intent(in) :: x
    
    answer = log(x/(1.0-x))
    
  end function logit
  
end module logistic


program main
  use logistic
  implicit none
  
  integer :: i, j, rows, columns
  real, allocatable :: x(:,:),l(:,:),e(:,:)
  logical, allocatable :: check(:,:)
  
  ! allocate desired array
  print '(A)', "Enter the number of rows and columns:"
  read(*,*) rows, columns
  allocate(x(rows,columns))
  allocate(l(rows,columns))
  allocate(e(rows,columns))
  allocate(check(rows,columns))
  
  ! fill array with entries
  print '(A)', "Initializing the matrix with entries..."
  print *
  call fill_with_uniforms(x) 		!need numbers between 0 and 1
  
  !print results
  print '(/,A)', "Original matrix:"
  do i=1,rows
     print '(100f10.4)', (x(i,j), j=1,columns)
  end do
  
  print '(/,A)',"Apply Logit() to matrix:"
  l=logit(x)
  do i=1,rows
     print '(100f10.4)', (l(i,j),j=1,columns)
  end do
  
  print '(/,A)', "Now apply Expit() ...:"
  e=expit(l)
  do i=1,rows
     print '(100f10.4)', (e(i,j),j=1,columns)
  end do
  
  ! check if original matrix recovered
  check = e == x
  
  if(all(check)) then
     print '(/,A)', "Original matrix recovered! All is well!"
  else
     print '(/,A)', "Something is amiss!"
  endif
  
  deallocate(e,l,x,check)
  
end program main
