module mat_stuff

	contains

    subroutine matprod(mat1, mat2, mprod, error_code)
	implicit none
    integer, intent(out) :: error_code
    real, intent(in) :: mat1(:,:), mat2(:,:)
    real, intent(out) :: mprod(:,:)

	! check for matrix conformity
	if(size(mat1,2) /= size(mat2,1)) then
		mprod=0.0
        error_code = 1
    else
		mprod = matmul(mat1, mat2)
        error_code = 0
    endif

    end subroutine matprod

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
  use mat_stuff
  implicit none
  integer :: i, j, rows1, columns1, rows2, columns2
  real :: a, b
  real, allocatable :: x(:,:), y(:,:)
  
  print "(A)", "Enter the lower an upper bounds:"
  read(*,*) a, b
  print "(A)", "How many rows and columns do you want for x? (max 100)"
  read(*,*) rows1, columns1
  print "(A)", "How many rows and columns do you want for y? (max 100)"
  read(*,*) rows2, columns2  
  
  allocate(x(rows1,columns1))
  allocate(y(rows2,columns2))
  
  call fill_with_uniforms(x, upper=b, lower=a)
  call fill_with_uniforms(y, upper=b, lower=a)
  

  print "(/,A)", "Here is x:"
  do i = 1, rows1
   print '(100f8.3)', (x(i,j), j = 1, columns1) 
  end do

  print "(/,A)", "Here is y:"
  do i = 1, rows2
   print '(100f8.3)', (y(i,j), j = 1, columns2) 
  end do
  
  deallocate(x,y)   ! not really necessary



end program mat_test