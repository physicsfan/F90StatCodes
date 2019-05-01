module mat_stuff

	contains

	real function binomial(weights, probability) result(answer)
	! This program uses allocatable arrays to compute and print the 
	! binomial probability mass function:
	!	
	!			f(y) = (n)p^y *(1 - p)^(n-y),		y=0,1,...,n
	!				   (y)
	!
  	implicit none
	! declare variables
    integer, intent(in) :: weights(:)
    real, intent(in), optional :: probability
    real :: answer(size(weights))
	! declare locals    
  	integer(kind=SELECTED_INT_KIND(9)) :: i, n, n_factorial, y, y_factorial, n_minus_y, n_minus_y_factorial
    real :: p

    p = 0.5
    if(present(probability)) p = probability
    n = size(weights)-1
      
	! calculate n-factorial
  	n_factorial=factorial(n)
  
  	do i=1,size(weights)
		! calculate y-factorial
  		y_factorial=factorial(weights(i))
		! calcualte (n-y)-factorial
  		n_minus_y=n-weights(i)
		n_minus_y_factorial=factorial(n-weights(i))

		! calculate f(y)
    	answer=n_factorial * (p**weights(i) * (1.0-p)**n-weights(i)) & 
        		/ (factorial(weights(i))*factorial(n-weights(i)))
  	end do

	end function binomial

    

    subroutine calculate_XtWX(matX, xtwx, weights)
    ! Subroutine that calculates the weighted sum of squares and cross products XtWX
    ! for input matrix X and a vector of weights (optional).  If weights are not present
    ! the subroutine calculates the product XtX.
	implicit none
    
	! declare variables
    real, intent(in) :: matX(:,:)
    real, intent(in), optional :: weights(:)
    real, intent(out) :: xtwx(:,:)
    ! declare locals
    integer :: i, j
    real :: wMatX(size(matX,1),size(matX,2))
    real :: matXT(size(matX,2),size(matX,1))
    real :: W(size(matX,1),size(matX,1))
	
	! create mstXt inialize W
    matXT = transpose(matX)
    W = 0.0
    wMatX = 0.0
    
	! create matrix of Weights
    if(present(weights)) then
    	do i = 1, size(weights)
      		W(i,i) = weights(i)
    	enddo
    else
    	do i = 1, size(W,1)
      		W(i,i) = 1.0
    	enddo
    end if      

	!XtWX is symmetric. Calculate the upper triangular portion only
	xtwx = 0.0
    wMatX = matmul(W,matX)

    do i = 1,size(matX,2)
      do j = i, size(matX,2)
      	xtwx(i,j) = dot_product(matXT(i,:),wMatX(:,j))
      end do
    end do
    
	!transpose the off-diagonal elements.
    do i = 1,size(matX,2)
      do j = i, size(matX,2)
      	xtwx(j,i) = xtwx(i,j)
      end do
    end do
    
	end subroutine calculate_XtWX


    subroutine fill_with_uniforms(mat, lower, upper)
    ! Fills the rank_one array vec with random numbers uniformly
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


    recursive integer function factorial(x) result(answer)
    	implicit none
        integer, intent(in) :: x
        if(x ==0) then
          answer = 1
        else
          answer = x * factorial(x-1)
        end if
    end function factorial

end module mat_stuff


program mat_test
  ! Main program to test the weighted_sum_of_squares_and_cross_products 
  ! function and other elements of the mat_stuff module.
  use mat_stuff
  implicit none
  
  ! declare locals
  integer :: i, j, xRows, xColumns, wElements
  real :: a, b
  character(len=3) :: boundsWanted, weightsPresent
  real, allocatable :: x(:,:), vectorOfWeights(:), matProd(:,:)
  
  call init_random_seed()	! initialize random number generator
  
  ! obtain sizes of input matrices
  print "(A)", "How many rows and columns do you want for x? (max 100)"
  read(*,*) xRows, xColumns
  
  wElements = xRows  
  
  ! allocate all matrices
  allocate(x(xRows,xColumns))
  allocate(vectorOfWeights(wElements))
  allocate(matProd(xColumns,xColumns))

  ! select if user wants numbers between a particular range
  print '(A)', "Do you want explicit lower and upper bounds?"
  read(*,*) boundsWanted
  
  ! fill input matrix X with values using a random number generator
  if((boundsWanted(1:1) == 'y') .or. (boundsWanted(1:1) == 'Y')) then
    print "(A)", "Enter the lower an upper bounds:"
  	read(*,*) a, b
    call fill_with_uniforms(x, upper=b, lower=a)
  else
    call fill_with_uniforms(x)
  endif

  print "(A)", "Do you have a vector of weights?"
  read(*,*) weightsPresent
  
  if((weightsPresent(1:1) == 'y') .or. (weightsPresent(1:1) == 'Y')) then
    
    ! generate the vector of weights
	do i=1,wElements
    	vectorOfWeights(i) = 1.0/real(wElements)
    end do
  	
    ! print out input matrix X 
  	print "(/,A)", "Here is X:"
  	do i = 1, xrows
    	print '(100f8.3)', (x(i,j), j = 1, xcolumns) 
  	end do

  	! print out the vector of weights
  	print "(/,A)", "Here is w:"
  	do i = 1, wElements
    	print '(100f8.3)', vectorOfWeights(i) 
  	end do

    ! calculate matrix product
    call calculate_XtWX(x,matProd,vectorOfWeights)
	
    ! print results
    print '(/,A,/)', "Success: The product XtWX..."
    do i = 1, size(matProd,1)
       print '(100f8.3)', (matProd(i,j), j = 1, size(matProd,2)) 
    end do  

  else
    
    ! No weights, print out input matrix X only 
  	print "(/,A)", "Here is X:"
  	do i = 1, xrows
    	print '(100f8.3)', (x(i,j), j = 1, xcolumns) 
  	end do
    
    ! calculate matrix product
    call calculate_XtWX(x,matProd)
    
	! print results
    print '(/,A,/)', "Success: The product XtX..."
    do i = 1, size(matProd,1)
       print '(100f8.3)', (matProd(i,j), j = 1, size(matProd,2)) 
    end do  
  end if
  
  deallocate(x, vectorOfWeights, matProd)   ! not really necessary

end program mat_test