module manipulate_string
  
contains
  
  integer function center_string(input_string) result(answer)
    ! Function accepts a character string of arbitrary length
    ! as input and centers the text within the string.
    ! Returns 0 if operation is successful, 1 otherwise.
    implicit none
    character(len=*), intent(inout) :: input_string
    ! declare locals
    integer :: i
    
    !if(suffix(1:1)=="") goto 5				! suffix begins with blank
    i = index(file_name, ".", back=.true.)  ! look for last period
    if(i==0) i = len_trim(file_name) + 1	! if no period was found
    if(len(file_name) < i+3) goto 5			! not enough room for suffix
    
    file_name(i:) = "." // suffix	! add suffix to file name
    
    ! normal exit
    answer = 0
    return
    ! error trap
5   answer = 1
    return       
  end function center_string
  
end module manipulate_string

!###########################################################################
program test_string
  use manipulate_string
  ! a main program to test the center_string function
  implicit none
  
  integer, parameter :: stringLengthMax = 80
  character(len=stringLengthMax) :: inputString = ""
  
  
  print "(A)", "Please enter an arbitrary character string:"
  read(*,*) inputString
  
  print "(A,A)", "Your original string is: ", inputString
  if (center_string(inputString) == 0) then
     print "(A,A)", "Your new centered string is: ", inputString
  else
     print "(A)", "There was an error"
  endif
  
end program test_string
