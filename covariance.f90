module covariance_mod

contains
  
  subroutine covariance(matX, covMat, kWeight)
    ! Subroutine that calculates the covariance matrix of an input
    ! data matrix matX given an 
    implicit none
    
    ! declare variables
    real, intent(in) :: matX(:,:)
    integer, intent(in), optional :: kWeight
    real, intent(out) :: covMat(:,:)
    ! declare locals
    integer :: i, j, k
    real :: ireal, kreal
    real :: deviation(1, size(matX,2))
    real :: deviationT(size(matX,2),1)
    real :: xBar(1,size(matX,2))
    real :: A(size(matX,2),size(matX,2))

    k = size(matX,1) - 1
    if(present(kWeight)) k = kWeight
    kreal = real(k)
    
    ! inialize xBar and A
    xBar = 0.0
    A = 0.0
    
    ! calculate xBar and A using one pass method
    do i = 1, size(matX,1)

       ireal = real(i)

       deviation(1,:) = matX(i,:) - xBar(1,:)
       deviationT = transpose(deviation)
       
       A = A + (((ireal-1.)/ireal)*(matmul(deviationT,deviation)))
       
       xBar = xBar + (1./ireal)*deviation
       
    end do
    
    ! calculate the covariance matrix from A
    covMat = A/kreal
    
  end subroutine covariance
  
  
  subroutine fill_with_uniforms(mat, lower, upper)
    ! Fills the array mat with random numbers uniformly
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
  
  
  subroutine init_random_seed()
    ! subroutine to initialize the random number generator
    
    ! declare locals
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
    
    ! initialize random seed with system clock
    call random_seed(size = n)
    allocate(seed(n))
    call system_clock(count=clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(PUT = seed)
    
    deallocate(seed)
    
  end subroutine init_random_seed

  
  subroutine observed(matX, bounds)
    ! Subroutine that generates a matrix of observed
    implicit none
    
    ! declare variables
    real, intent(out) :: matX(:,:)
    character(len=*), intent(in), optional :: bounds

    ! declare locals
    real :: a, b

    if(present(bounds)) then
       print "(A)", "Enter the lower an upper bounds:"
       read(*,*) a, b
       call fill_with_uniforms(matX, upper=b, lower=a)
    else
       call fill_with_uniforms(matX)
    endif
    
  end subroutine observed


  
end module covariance_mod


program covariance_main
  ! Main program to test the covariance module.
  use covariance_mod
  implicit none

  
  ! declare locals
  integer :: i, j, k, xRows, xColumns
  real :: a, b
  character(len=3) :: boundsWanted, kWanted
  real, allocatable :: x(:,:), cov_mat(:,:)
  

  ! initialize random number generator  
  call init_random_seed()	 
  

  ! obtain sizes of input matrices
  print "(A)", "How many rows and columns do you want for x? (max 100)"
  read(*,*) xRows, xColumns
  
  
  ! allocate all matrices
  allocate(x(xRows,xColumns))
  allocate(cov_mat(xColumns,xColumns))
  
  
  ! select if user wants numbers between a particular range
  print '(A)', "Do you want explicit lower and upper bounds?"
  read(*,*) boundsWanted

  
  ! fill input matrix X with values using a random number generator
  if((boundsWanted(1:1) == 'y') .or. (boundsWanted(1:1) == 'Y')) then
     call observed(x, 'y')
  else
     call observed(x)
  endif

  
  ! select k value (n or n-1):
  print '(/,A)', "Is the value of k = n?"
  read (*,*) kWanted
  if((kWanted == 'y') .or. (kWanted == 'Y')) then
     call covariance(x, cov_mat, xRows)
  else
     call covariance(x, cov_mat)
  end if

  
  ! Print results 
  print "(/,A)", "Here is X:"
  do i = 1, xrows
     print '(100f8.3)', (x(i,j), j = 1, xcolumns) 
  end do

  print '(/,A,/)', "The Covariance matrix of X:"
  do i = 1, size(cov_mat,1)
     print '(100f8.3)', (cov_mat(i,j), j = 1, size(cov_mat,2)) 
  end do

  
  deallocate(x, cov_mat)   ! not really necessary

  
end program covariance_main
