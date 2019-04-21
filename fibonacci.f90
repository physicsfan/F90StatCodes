module my_name_is_mr_fibbs

	contains

    recursive integer function fibonacci(x) result(answer)
    implicit none
    integer, intent(in) :: x
    
	select case(x)
	  case(0)
      	answer = 0
      case(1)
      	answer = 1
      case default  
      	answer = fibonacci(x-1) + fibonacci(x-2)
    end select
    end function fibonacci

end module

program create_fibonacci
  use my_name_is_mr_fibbs
  implicit none

  integer :: i, n

  print '(A)', "Please enter an integer:"
  read(*,*) n
 
  do i=0,n
  	print '(A,i2,A,i6)', "The ", i,"-th element in the sequence is: ", fibonacci(i)
  enddo

  end