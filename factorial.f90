module show_me_the_facs
contains
  
  recursive integer function factorial(x) result(answer)
    implicit none
    integer, intent(in) :: x
    
    if(x == 0) then
       answer = 1
    else
       answer = x * factorial(x-1)
    end if
  end function factorial
  
end module show_me_the_facs

program just_the_facs
  use show_me_the_facs
  ! program to test the recursive factorial function 
  implicit none
  
  integer :: n
  
  print '(A)', "Please enter an integer:"
  read(*,*) n
  
  if(n > 9) then
     print '(A)', "n is TOO BIG!"      ! test overflow of integers
  else  
     print '(/,i2,A,i6)', n, "! is: ", factorial(n)
  endif
  
end program just_the_facs
