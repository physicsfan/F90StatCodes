module variance_mod

  private
  
  integer, parameter :: dp = selected_real_kind(15,307)

  public :: dp, fill_with_uniforms, get_sample, get_variance, init_random_seed

  interface get_sample
     ! generic procedure to obtain a data sample as either a vector
     ! or a 2d matrix
     module procedure get_mat
     module procedure get_vec
  end interface get_sample

  interface get_variance
     ! generic procedure to calculate either the sample variance
     ! or covariance depending on whether the data is a vector
     ! or a matrix     
     module procedure sample_variance
     module procedure sample_covariance
  end interface get_variance

  interface fill_with_uniforms
     module procedure fill_mat
     module procedure fill_vec
  end interface fill_with_uniforms
  
contains  
  
  subroutine fill_mat(mat, lower, upper)
    ! Fills the array mat with random numbers uniformly
    ! distributed from lower to upper, which default to 0.0 and
    ! 1.0, respectively
    implicit none
    
    ! declare arguments
    real(dp), intent(out) :: mat(:,:)
    real(dp), intent(in), optional :: lower, upper
    
    ! declare locals
    real(dp) :: a, b
    
    ! begin
    a = 0.0
    b = 1.0
    if(present(lower)) a = lower
    if(present(upper)) b = upper
    call random_number(mat)
    mat = a + mat * (b - a)
    
  end subroutine fill_mat

  subroutine fill_vec(mat, lower, upper)
    ! Fills the vector mat with random numbers uniformly
    ! distributed from lower to upper, which default to 0.0 and
    ! 1.0, respectively
    implicit none
    
    ! declare arguments
    real(dp), intent(out) :: mat(:)
    real(dp), intent(in), optional :: lower, upper
    
    ! declare locals
    real(dp) :: a, b
    
    ! begin
    a = 0.0
    b = 1.0
    if(present(lower)) a = lower
    if(present(upper)) b = upper
    call random_number(mat)
    mat = a + mat * (b - a)
    
  end subroutine fill_vec
  
  
  subroutine get_mat(matX, bounds)
      ! Subroutine that generates a matrix of observed
      implicit none
    
    ! declare variables
    real(dp), intent(out) :: matX(:,:)
    character(len=*), intent(in), optional :: bounds
    
    ! declare locals
    real(dp) :: a, b
    
    if(present(bounds)) then
       print "(A)", "Enter the lower an upper bounds:"
       read(*,*) a, b
       call fill_with_uniforms(matX, upper=b, lower=a)
    else
       call fill_with_uniforms(matX)
    endif
    
  end subroutine get_mat
  
  subroutine get_vec(vecX, bounds)
    ! Subroutine that generates a matrix of observed
    implicit none
    
    ! declare variables
    real(dp), intent(out) :: vecX(:)
    character(len=*), intent(in), optional :: bounds
    
    ! declare locals
    real(dp) :: a, b
    
    if(present(bounds)) then
       print "(A)", "Enter the lower an upper bounds:"
       read(*,*) a, b
       call fill_with_uniforms(vecX, upper=b, lower=a)
    else
       call fill_with_uniforms(vecX)
    endif
    
  end subroutine get_vec
  
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

  
  subroutine sample_covariance(matX, covMat)
    ! Subroutine that calculates the covariance matrix of an input
    ! data matrix matX
    implicit none
    
    ! declare variables
    real(dp), intent(in) :: matX(:,:)
    real(dp), intent(out) :: covMat(:,:)
    ! declare locals
    integer :: i, j, k
    real(dp) :: ireal, kreal
    real(dp) :: deviation(1, size(matX,2))
    real(dp) :: deviationT(size(matX,2),1)
    real(dp) :: xBar(1,size(matX,2))
    real(dp) :: A(size(matX,2),size(matX,2))

    k = size(matX,1) - 1
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
    
  end subroutine sample_covariance

  
  subroutine sample_variance(matX, variance)
    ! Subroutine that calculates the sample variance
    ! of an input data vector 
    implicit none
    
    ! declare variables
    real(dp), intent(in) :: matX(:)
    real(dp), intent(out) :: variance
    ! declare locals
    integer :: i, j, k, n
    real(dp) :: xBar

    k = size(matX,1) - 1
    n = size(matX,1)
    
    ! calculate xBar and variance
    xBar = sum(matX)/n
    variance = sum((matX - xBar)**2)/k
    
  end subroutine sample_variance

  
end module variance_mod


program variance_main
  ! Main program to test the variance module.
  use variance_mod
  implicit none

  
  ! declare locals
  integer :: i, j, k, xRows, xColumns
  real(dp) :: a, b, sampleVariance
  character(len=3) :: boundsWanted, kWanted
  real(dp), allocatable :: x(:,:), xVec(:), cov_mat(:,:)
  

  ! initialize random number generator  
  call init_random_seed()	 
  

  ! obtain sizes of input matrices
  print "(A)", "How many rows and columns do you want for x? (max 100)"
  read(*,*) xRows, xColumns
  
  
  ! allocate all matrices
  allocate(x(xRows,xColumns))
  allocate(xVec(xColumns))
  allocate(cov_mat(xColumns,xColumns))
  
  
  ! select if user wants numbers between a particular range
  print '(A)', "Do you want explicit lower and upper bounds?"
  read(*,*) boundsWanted

  
  ! fill input matrix X with values using a random number generator
  if((boundsWanted(1:1) == 'y') .or. (boundsWanted(1:1) == 'Y')) then
     call get_sample(x, 'y')
     call get_sample(xVec, 'y')
  else
     call get_sample(x)
     call get_sample(xVec)
  endif

  
  ! calculate sample variance for xVec and covariance for x
  call get_variance(x, cov_mat)
  call get_variance(xVec, sampleVariance)

  
  ! Print results 
  print "(/,A)", "Here is X:"
  do i = 1, xrows
     print '(100f8.4)', (x(i,j), j = 1, xcolumns) 
  end do
  
  print '(/,A)', "The Covariance matrix of X:"
  do i = 1, size(cov_mat,1)
     print '(100f8.4)', (cov_mat(i,j), j = 1, size(cov_mat,2)) 
  end do
  
  print "(/,A)", "Here is xVec:"
  print '(100f8.4)', (xVec(j), j = 1, xcolumns) 
  
  print '(/,A,f8.4)', "The sample variance of xVec:", sampleVariance 

  
  deallocate(x, xVec, cov_mat)   ! not really necessary

  
end program variance_main
