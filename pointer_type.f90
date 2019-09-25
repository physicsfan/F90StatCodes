module pointer_mat
  implicit none
  
  private                               ! by default

  ! set precision
  integer, parameter :: sp = kind(0.0)
  integer, parameter :: dp = kind(0.d0)

  ! explicit public types
  public :: sp
  public :: pointer_type

  ! define derived type
  type :: pointer_type
     real(sp), pointer :: aptr => null()
  end type pointer_type
  
end module pointer_mat


program mat_test
  ! Main program to test the creation of an array of pointers using a
  ! derived type
  use pointer_mat
  implicit none
  
  ! declare locals
  integer :: i, j, xRows, xColumns
  real(sp), allocatable, target :: x(:,:)
  type(pointer_type), allocatable :: array_of_pointers(:,:)
  
  ! obtain sizes of input matrices
  xRows=5; xColumns=5
  
  ! allocate all matrices
  allocate(x(xRows,xColumns))
  allocate(array_of_pointers(xRows,xColumns))
  
  ! fill matrix with values
  do i = 1, xColumns
     x(:,i) = real(i)
  end do

  ! associate pointer array
  do i = 1, xRows
     do j = 1, xColumns
        array_of_pointers(i,j)%aptr => x(i,j)
     end do
  end do

  ! print input matrix x
   print "(/,A)", "Here is x:"
   do i = 1, xrows
      print '(100f8.3)', (x(i,j), j = 1, xColumns) 
   end do
   
   ! print array of pointers
   print '(/,A,/)', "Here is the array of pointers"
   do i = 1, xRows
      print '(100f8.3)', (array_of_pointers(i,j)%aptr, j = 1, xColumns) 
   end do

   ! change the second row to 7.0 using the array of pointers
   do j = 1, xColumns
      array_of_pointers(2,j)%aptr = 7.
   end do
   
   ! print input matrix x
   print "(/,A)", "Here is x:"
   do i = 1, xrows
      print '(100f8.3)', (x(i,j), j = 1, xColumns) 
   end do

deallocate(x, array_of_pointers)   ! not really necessary

end program mat_test
