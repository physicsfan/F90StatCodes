module mat_stuff

	contains

    integer function mat_prod(mat1, mat2, mprod) result(error_code)
    ! function that calculate the matrix product of input matrices.
    ! function checks to see if the sizes of the inputs are correct.
	implicit none
    
	! declare variables
    integer :: error_code
    real, intent(in) :: mat1(:,:), mat2(:,:)
    real, intent(out) :: mprod(:,:)
	
	! check for matrix conformity
    ! Note: goto only used for error traps
	if(size(mat1,2) /= size(mat2,1)) goto 1
    
	! normal return
	mprod = matmul(mat1, mat2)
    error_code = 0
    return    	
	
	! error trap
1	continue    
	mprod=0.0
    error_code = 1
    return
    
	end function mat_prod


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

end module mat_stuff


program mat_test
  ! Main program to test the mat_prod function and other elements of the 
  ! mat_stuff module.
  use mat_stuff
  implicit none
  
  ! declare locals
  integer :: i, j, xrows, xcolumns, yrows, ycolumns
  real :: a, b
  character(len=3) :: boundsWanted
  real, allocatable :: x(:,:), y(:,:), matProd(:,:)
  
  call init_random_seed()	! initialize random number generator
  
  ! obtain sizes of input matrices
  print "(A)", "How many rows and columns do you want for x? (max 100)"
  read(*,*) xrows, xcolumns
  print "(A)", "How many rows and columns do you want for y? (max 100)"
  read(*,*) yrows, ycolumns  
  
  ! allocate all matrices
  allocate(x(xrows,xcolumns))
  allocate(y(yrows,ycolumns))
  allocate(matProd(xrows,ycolumns))

  ! select if user wants numbers between a particular range
  print '(A)', "Do you want explicit lower and upper bounds?"
  read(*,*) boundsWanted
  
  ! fill input matrices with values using a random number generator
  if((boundsWanted(1:1) == 'y') .or. (boundsWanted(1:1) == 'Y')) then
    print "(A)", "Enter the lower an upper bounds:"
  	read(*,*) a, b
    call fill_with_uniforms(x, upper=b, lower=a)
    call fill_with_uniforms(y, upper=b, lower=a)
  else
    call fill_with_uniforms(x)
    call fill_with_uniforms(y)
  endif
  
  ! print out input matrices, X and Y
  print "(/,A)", "Here is x:"
  do i = 1, xrows
   print '(100f8.3)', (x(i,j), j = 1, xcolumns) 
  end do
  print "(/,A)", "Here is y:"
  do i = 1, yrows
   print '(100f8.3)', (y(i,j), j = 1, ycolumns) 
  end do
  
  ! calculate matrix product, return error if input matrices do not
  ! have the correct bounds
  select case(mat_prod(x, y, matProd))
  case(1)
	print '(/,A,/)', "Error: The input matrix sizes do not conform..."
  	do i = 1, size(matProd,1)
   		print '(100f8.3)', (matProd(i,j), j = 1, size(matProd,2)) 
  	end do
  case default
	print '(/,A,/)', "Success: The product of x and y is..."
  	do i = 1, size(matProd,1)
   		print '(100f8.3)', (matProd(i,j), j = 1, size(matProd,2)) 
  	end do	
  end select
  
  deallocate(x,y,matProd)   ! not really necessary

end program mat_test