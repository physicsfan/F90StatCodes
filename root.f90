module mymod
	contains
    elemental real function square_root(x) result(answer)
    implicit none
    real, intent(in) :: x
    real :: old
    answer = 1.
    do
      old = answer
      answer = (old + x/old)/2.
      if(answer == old) exit
    end do
    end function square_root
end module mymod


program root
use mymod

implicit none

integer :: i, n
real, allocatable :: mat(:)

print '(A)', "Enter the number of elements:"
read(*,*) n

allocate(mat(n))

print '(A)', "Initializing the matrix with squares..."
print *

mat=(/(i*i, i=1,n)/)   !initialize with an implied do loop
	

print *, square_root(mat)

deallocate(mat)

end program root