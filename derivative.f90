module d_mod

contains
    
  subroutine derivative(func, vec, delta)
    implicit none
    ! declare arguments
    real, intent(inout) :: vec(:)
    real, intent(in), optional :: delta
    ! declare locals
    integer :: i, n
    real :: d
    real :: func
    ! begin
    d = 0.0001
    n = size(vec)
    if(present(delta)) d = delta
    do i=1,n
       vec(i) = (func(vec(i)+d/2.0)-func(vec(i)-d/2.0))/d
    enddo
  end subroutine derivative
  
  
  real function f(x) result(answer)
    implicit none
    real, intent(in) :: x
    
    answer = x-exp(-x)
    
  end function f
  
  
  elemental real function df(x) result(answer)
    implicit none
    real, intent(in) :: x
    
    answer = 1+exp(-x)
    
  end function df
  
end module d_mod


program main
  use d_mod
  implicit none
  
  integer :: i, n
  real, allocatable :: grid(:),fgrid(:),dfgrid(:), residuals(:)
  
  print '(A)', "Enter the number of elements:"
  read(*,*) n
  
  allocate(grid(n))
  allocate(fgrid(n))
  allocate(dfgrid(n))
  allocate(residuals(n))
  
  print '(A)', "Initializing the matrix with squares..."
  print *
  
  grid=(/(0.5*i, i=1,n)/)   !initialize with an implied do loop
  
  fgrid=grid
  
  call derivative(f, fgrid)
  dfgrid=df(grid)
  residuals=abs(fgrid-dfgrid)
  
  print *, grid
  print *, (fgrid(i), i=1,n) 
  print *, (dfgrid(i), i=1,n)
  print *, (residuals(i), i=1,n)
  
  deallocate(dfgrid,fgrid,grid,residuals)
  
end program main
