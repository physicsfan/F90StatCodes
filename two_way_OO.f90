module class_Two_Way_Table

  integer, parameter :: dp = selected_real_kind(15,307)

  type :: two_way_table_type
  	private
    real(dp), pointer :: observed(:,:) => null()
    real(dp), pointer :: expected(:,:) => null()
    integer			  :: df
    real(dp)		  :: Pearson
    real(dp)		  :: deviance
  end type two_way_table_type

contains
  
! ==============================Setter Functions==================================
  subroutine put_observed(matX, results)
    ! Subroutine that generates a matrix of observed
    implicit none
    ! declare variables
    real(dp), pointer :: matX(:,:)
    type(two_way_table_type), intent(out) :: results
    ! locals
    integer :: n, p
    ! begin
    n = size(matX,1); p = size(matX,2)
    if(associated(results%observed)) deallocate(results%observed)
    allocate(results%observed(n,p))
    results%observed(:,:) = matX(:,:)
  end subroutine put_observed


   integer function run_chisquare_test(results) result(answer)
    ! Given a full-rank nxp data matrix X, the function transforms it into
    ! another nxp matrix whose columns span the same linear space but are
    ! mutually orthogonal using the Modified Gram-Schmidt (MGS) procedure.
    ! The function checks if X will be rank deficient returning 0 for normal
    ! exit and 1 if ranks are incorrect
    implicit none
    ! declare variables
    type(two_way_table_type) :: results
    ! declare locals
    integer :: r, c, i, j
    real(dp) :: deviance, pearson, xpp, zero
    ! initiallize
    r = size(results%observed,1); c = size(results%observed,2)
    if(associated(results%expected)) deallocate(results%expected)
    allocate(results%expected(r,c))
    zero = 0.0e-35
    pearson  = zero
    deviance = zero
    ! check if x is rank deficient
    if ((r < 2) .or. (c < 2)) goto 5
    ! check any element of x is negative
    if (any(results%observed < zero)) goto 6
    ! calculate degrees of freedom snd expected counts
    results%df = (r - 1) * (c - 1)
    xpp = sum(results%observed(:,:))
    do i = 1, r
       do j = 1, c
          results%expected(i,j) = (sum(results%observed(i,:))*sum(results%observed(:,j)))/xpp
       end do
    end do
    ! check if any expected counts are equal to zero
    if(any(results%expected <= zero)) goto 7
    ! calculate Pearson and Deviance statistics
    do i = 1, r
       do j = 1, c
          pearson = pearson + (results%expected(i,j)-results%observed(i,j))**2 &
          				/results%expected(i,j)
          deviance = deviance + 2.0*(results%observed(i,j)*log(results%observed(i,j) &
          				/results%expected(i,j)))
       end do
    end do
	results%Pearson = pearson
	results%deviance = deviance
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

! ====================Getter functions==================================
  subroutine get_two_way_expected(expected, results)
  	! For getting the expected values
    implicit none
    ! arguments
    real(dp), pointer :: expected(:,:)
    type(two_way_table_type), intent(in) :: results
    ! locals
    integer :: n, p
    ! begin
    n = size(results%expected,1); p = size(results%expected,2)
    if(associated(expected)) deallocate(expected)
    allocate(expected(n,p))
    expected(:,:) = results%expected(:,:)
  end subroutine get_two_way_expected

  subroutine get_two_way_df(df, results)
  	! For getting the df
    implicit none
    ! arguments
    integer, intent(out) :: df
    type(two_way_table_type), intent(in) :: results
    ! begin
    df = results%df
  end subroutine get_two_way_df

  subroutine get_two_way_deviance(deviance, results)
  	! For getting the deviance
    implicit none
    ! arguments
    real(dp), intent(out) :: deviance
    type(two_way_table_type), intent(in) :: results
    ! begin
    deviance = results%deviance
  end subroutine get_two_way_deviance

  subroutine get_two_way_Pearson(Pearson, results)
  	! For getting the deviance
    implicit none
    ! arguments
    real(dp), intent(out) :: Pearson
    type(two_way_table_type), intent(in) :: results
    ! begin
    Pearson = results%Pearson
  end subroutine get_two_way_Pearson  
  
end module class_Two_Way_Table

!========================== Test Program ===============================
program two_way_table_main
  ! Main program to test the two_way_table module.
  use class_Two_Way_Table
  implicit none
  
  ! declare locals
  type(two_way_table_type) 		:: two_way_table
  integer 						:: df, i, j, k, xRows, xColumns
  real(dp) 						:: a, b, pearson, deviance
  character(len=3) 				:: boundsWanted
  real(dp), allocatable, target :: x(:,:)
  real(dp), allocatable         :: expected(:,:)
  
  ! initialize random number generator  
  call init_random_seed()	 
  
  ! obtain sizes of input matrices
  print "(A)", "How many rows and columns do you want for x? (max 100)"
  read(*,*) xRows, xColumns
  
  ! allocate all matrices
  allocate(x(xRows,xColumns))
  
  ! select if user wants numbers between a particular range
  print '(A)', "Do you want explicit lower and upper bounds?"
  read(*,*) boundsWanted

  ! fill input matrix X with values using a random number generator
  if((boundsWanted(1:1) == 'y') .or. (boundsWanted(1:1) == 'Y')) then
     call observed(x, 'y')
  else
     call observed(x)
  endif

  ! Insert X into two_way_table
  call put_observed(x, two_way_table)
  
  ! calculate two-way table statistics, print error message if error occurs
  select case(run_chisquare_test(two_way_table))
  case(1)
     print "(/,A)", "Rank of X is difficient"
  case(2)
     print "(/,A)", "There is a negative element in X"     
  case(3)
     print "(/,A)", "An expected count is zero"
  case default
     ! Obtain and Print results
     call get_two_way_expected(expected, two_way_table)
     call get_two_way_df(df, two_way_table)
     call get_two_way_deviance(deviance, two_way_table)
     call get_two_way_Pearson(pearson, two_way_table)
      
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

contains
  
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


  subroutine fill_with_uniforms(mat, lower, upper)
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
  end subroutine fill_with_uniforms


  subroutine observed(matX, bounds)
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
  end subroutine observed
  
end program two_way_table_main
