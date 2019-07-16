module Gram_Schmidt

  integer, parameter :: dp = selected_real_kind(15,307)
  
contains  
  
  integer function mgs(X,Q,R) result(answer)
    ! Given a full-rank nxp data matrix X, the function transforms it into
    ! another nxp matrix whose columns span the same linear space but are
    ! mutually orthogonal using the Modified Gram-Schmidt (MGS) procedure.
    ! The function checks if X will be rank deficient returning 0 for normal
    ! exit and 1 if ranks are incorrect
    implicit none
    
    ! declare variables
    real(dp), intent(in) :: X(:,:)
    real(dp), intent(out) :: Q(:,:)
    real(dp), intent(out) :: R(:,:)
    ! declare locals
    integer :: n, p, j, k

    n = size(X,1); p = size(X,2) 
    R(:,:) = 0.0
    Q(:,:) = X(:,:)

    ! check if X is rank deficient
    if ((n < 2) .or. (p < 2)) then
       goto 5   
    else
    ! calculate Q using MGS procedure
       do j = 1, p
          R(j,j) = sqrt(sum(Q(:,j)**2))
          Q(:,j) = Q(:,j) / R(j,j)
          do k = (j+1), p
             R(j,k) = dot_product(Q(:,j),Q(:,k))
             Q(:,k) = Q(:,k) - Q(:,j)*R(j,k)
          end do
       end do
    end if
    ! normal exit
    answer = 0
    return
    
    ! error trap
5   answer = 1
    
  end function mgs
  
  
  subroutine fill_with_uniforms(mat, lower, upper)
    ! Fills the rank_one array vec with random numbers uniformly
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

  
end module Gram_Schmidt


program mgs_main
  ! Main program to test the Gram_Schmidt module.
  use Gram_Schmidt
  implicit none

  
  ! declare locals
  integer :: i, j, k, xRows, xColumns
  real(dp) :: a, b
  character(len=3) :: boundsWanted, kWanted
  real(dp), allocatable :: xMat(:,:), qMat(:,:), rMat(:,:), iMat(:,:), xTest(:,:) 
  

  ! initialize random number generator  
  call init_random_seed()	 
  

  ! obtain sizes of input matrices
  print "(A)", "How many rows and columns do you want for x? (max 100)"
  read(*,*) xRows, xColumns
  
  
  ! allocate all matrices
  allocate(xMat(xRows,xColumns))
  allocate(qMat(xRows,xColumns))
  allocate(rMat(xColumns,xColumns)) 
  allocate(iMat(xRows,xColumns))
  allocate(xTest(xRows,xColumns))
  
  
  ! select if user wants numbers between a particular range
  print '(A)', "Do you want explicit lower and upper bounds?"
  read(*,*) boundsWanted

  
  ! fill input matrix X with values using a random number generator
  if((boundsWanted(1:1) == 'y') .or. (boundsWanted(1:1) == 'Y')) then
     print "(A)", "Enter the lower an upper bounds:"
     read(*,*) a, b
     call fill_with_uniforms(xMat, upper=b, lower=a)
  else
     call fill_with_uniforms(xMat)
  endif

  
 
  if(mgs(xMat,qMat,rMat) > 0) then
     print "(/,A)", "Rank of X is difficient"
  else
     ! Print results 
     print "(/,A)", "Here is X:"
     do i = 1, xRows
        print '(100f10.6)', (xMat(i,j), j = 1, xColumns) 
     end do
     
     print "(/,A)", "Here is Q:"
     do i = 1, xRows
        print '(100f10.6)', (qMat(i,j), j = 1, xColumns) 
     end do
     
     print "(/,A)", "Here is R:"
     do i = 1, xColumns
        print '(100f10.6)', (rMat(i,j), j = 1, xColumns) 
     end do
     
     print '(/,A)', "Test Q:"
     print "(/,A)", "Does Qt.Q ~ I:"
     iMat = matmul(transpose(qMat),qMat)
     do i = 1, size(iMat,1)
        print '(100f10.6)', (iMat(i,j), j = 1, size(iMat,2)) 
     end do

     print "(/,A)", "Does Q.R ~ X:"
     do i = 1, size(xMat,1)
        xTest = matmul(qMat,rMat)
        print '(100f10.6)', (xTest(i,j), j = 1, size(xMat,2)) 
     end do

  end if
  
  deallocate(xMat, qMat, rMat, iMat, xTest)   ! not really necessary
  
end program mgs_main
