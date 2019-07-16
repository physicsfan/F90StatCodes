module two_way_table

  integer, parameter :: dp = selected_real_kind(15,307)

contains

  
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


   integer function run_chisquare_test(xMat,xHat,degFreedom,xSquared,gSquared) result(answer)
    ! Given a full-rank nxp data matrix X, the function transforms it into
    ! another nxp matrix whose columns span the same linear space but are
    ! mutually orthogonal using the Modified Gram-Schmidt (MGS) procedure.
    ! The function checks if X will be rank deficient returning 0 for normal
    ! exit and 1 if ranks are incorrect
    implicit none
    
    ! declare variables
    real, intent(in) :: xMat(:,:)
    real, intent(out) :: xHat(:,:)
    real, intent(out) :: xSquared
    real, intent(out) :: gSquared
    integer,  intent(out) :: degFreedom
    ! declare locals
    integer :: r, c, i, j
    real :: xpp ,zero, x_2, g_2
    
    ! initiallize locals
    r = size(xMat,1); c = size(xMat,2)
    zero = 0.0e-35
    xHat(:,:) = zero
    xSquared = zero
    gSquared = zero
    x_2 = zero
    g_2 = zero

    ! check if x is rank deficient
    if ((r < 2) .or. (c < 2)) goto 5
    ! check any element of x is negative
    if (any(xMat < zero)) goto 6
    
    ! calculate degrees of freedom snd expected counts
    degFreedom = (r - 1) * (c - 1)
    xpp = sum(xMat(:,:))
    do i = 1, r
       do j = 1, c
          xHat(i,j) = (sum(xMat(i,:))*sum(xMat(:,j)))/xpp
       end do
    end do

    ! check if any expected counts are equal to zero
    if(any(xHat <= zero)) goto 7

    ! calculate Pearson and Deviance statistics
!!$    do i = 1, r
!!$       do j = 1, c
!!$          x_2 = x_2 + (xHat(i,j) - xMat(i,j))**2/xHat(i,j)
!!$          g_2 = g_2 + 2.0*(xMat(i,j)*log(xMat(i,j)/xHat(i,j)))
!!$       end do
!!$    end do
!!$    
!!$    print "(/,A,f10.6)", "The X^2 statistic is:", x_2
!!$    print "(/,A,f10.6)", "The G^2 statistic is:", g_2    

    
    xSquared = sum((xHat - xMat)**2/xHat)    ! use vectorization of F90
    gSquared = 2.0*sum(xMat*log(xMat/xHat))  ! "        "        "   "
    
    ! normal exit
    answer = 0
    return
    
    ! error trap
5   answer = 1       ! x is rank deficient
    return
6   answer = 2       ! elements of x are negative
    return
7   answer = 3       ! an expected count is zero
    return
    
  end function run_chisquare_test
  
end module two_way_table


program two_way_table_main
  ! Main program to test the two_way_table module.
  use two_way_table
  implicit none
  
  ! declare locals
  integer :: df, i, j, k, xRows, xColumns
  real :: a, b, pearson, deviance
  character(len=3) :: boundsWanted
  real, allocatable :: x(:,:), expected(:,:)
  
  
  ! initialize random number generator  
  call init_random_seed()	 
  
  
  ! obtain sizes of input matrices
  print "(A)", "How many rows and columns do you want for x? (max 100)"
  read(*,*) xRows, xColumns
  
  
  ! allocate all matrices
  allocate(x(xRows,xColumns))
  allocate(expected(xColumns,xColumns))
  
  
  ! select if user wants numbers between a particular range
  print '(A)', "Do you want explicit lower and upper bounds?"
  read(*,*) boundsWanted

  
  ! fill input matrix X with values using a random number generator
  if((boundsWanted(1:1) == 'y') .or. (boundsWanted(1:1) == 'Y')) then
     call observed(x, 'y')
  else
     call observed(x)
  endif

  ! calculate two-way table statistics, print error message if error occurs
  select case(run_chisquare_test(x,expected,df,pearson,deviance))
  case(1)
     print "(/,A)", "Rank of X is difficient"
  case(2)
     print "(/,A)", "There is a negative element in X"     
  case(3)
     print "(/,A)", "An expected count is zero"
  case default
     ! Print results 
     print "(/,A)", "Here is X:"
     do i = 1, xRows
        print '(100f10.6)', (x(i,j), j = 1, xColumns) 
     end do
     
     print "(/,A)", "Here are the expected counts (X^):"
     do i = 1, xRows
        print '(100f10.6)', (expected(i,j), j = 1, xColumns) 
     end do
     
     print "(/,A,i3)", "The degrees of freedom are:", df
     
     print "(/,A,f10.6)", "The Pearson statistic is:", pearson
     
     print "(/,A,f10.6)", "The deviance statistic is:", deviance
     
  end select
  

end program two_way_table_main
