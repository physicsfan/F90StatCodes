module manipulate_string

	contains

    integer function center_text(input_string) result(answer)
    	! Function accepts a character string of arbitrary length
        ! as input and centers the text within the string.
    	! Returns 0 if operation is successful, 1 otherwise.
        implicit none
        character(len=*), intent(inout) :: input_string
        ! declare locals
        integer :: i, trailing_blank_number, leading_blank_number

        ! input string diagnostics
		if(input_string(1:1)=="") input_string = adjustl(input_string)			! string begins with blank
        if(len_trim(input_string) == len(input_string)) goto 5					! not enough room for blanks
          
		! count number of leading and trailing blanks
        trailing_blank_number = len(input_string)-len_trim(input_string)
        leading_blank_number = trailing_blank_number/2
		
		print "(/,A,i3)", "Trailing blanks: ", trailing_blank_number
        print "(A,i3,/)", "Leading blanks: ", leading_blank_number

        ! insert appropriate number of leading blanks to center the string
		do i = 1,leading_blank_number		
			input_string = " "//input_string
        end do
        
		! normal exit
        answer = 0
        return
        ! error trap
    5   answer = 1
     	return       
	end function center_text

end module manipulate_string

!###########################################################################
program test_string
use manipulate_string
! a main program to test the add_suffix function
implicit none

integer, parameter :: stringLengthMax = 15
character(len=stringLengthMax) :: inputString = ""


print '(A)', "Please enter an arbitrary character string:"
read(*,'(A)') inputString

print '(/,A,A)', "Your original string is:", inputString

select case(center_text(inputString))
  case(1)
	print '(A)', "Error: There is not enough room for blanks"
  case default
    print '(/,A,A)', "Your new centered string is:", inputString 	
  end select
   
end program test_string