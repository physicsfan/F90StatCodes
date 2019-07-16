module manipulate_text

	contains

    integer function change_case(input_string, case_wanted) result(answer)
    	! Function accepts a character string of arbitrary length
        ! as input and returns upper-case letters from lower-case letters
        implicit none
        character(len=*), intent(inout) :: input_string
        character(len=5), intent(in) :: case_wanted
        ! declare locals
        integer :: i, ibig=iachar('A'), ismall=iachar('a')

        
		select case(case_wanted(1:1))
  		 case('u')
        	! convert lower case to upper case
			do i = 1, len(input_string)
        		if(input_string(i:i) >= 'a' .and. input_string(i:i) <= 'z') then		
					input_string(i:i) = char(iachar(input_string(i:i)) + ibig - ismall)
            	end if
        	end do
            ! normal exit
        	answer = 0
        	return
  		 case('U')
        	! convert lower case to upper case
			do i = 1, len(input_string)
        		if(input_string(i:i) >= 'a' .and. input_string(i:i) <= 'z') then		
					input_string(i:i) = char(iachar(input_string(i:i)) + ibig - ismall)
            	end if
        	end do
            ! normal exit
        	answer = 0
        	return
         case('l')
			! convert upper case to lower case
			do i = 1, len(input_string)
        		if(input_string(i:i) >= 'A' .and. input_string(i:i) <= 'Z') then		
					input_string(i:i) = char(iachar(input_string(i:i)) - ibig + ismall)
            	end if
        	end do
            ! normal exit
        	answer = 0
        	return
         case('L')
			! convert upper case to lower case
			do i = 1, len(input_string)
        		if(input_string(i:i) >= 'A' .and. input_string(i:i) <= 'Z') then		
					input_string(i:i) = char(iachar(input_string(i:i)) - ibig + ismall)
            	end if
        	end do
            ! normal exit
        	answer = 0
        	return            
		 case default
    		! error trap
    	   	answer = 1
     		return 	
  		end select
     
	end function change_case

end module manipulate_text

!####################################################################################
program test_upper
use manipulate_text
! a main program to test the reverse_text function
implicit none

integer, parameter :: stringLengthMax = 128
character(len=stringLengthMax) :: inputString = ""
character(len=5) :: caseWanted


print '(A)', "Please enter an arbitrary character string:"
read(*,'(A)') inputString

print '(A)', "Do you want an upper-case (upper) or lower-case (lower) string:"
read(*,'(A)') caseWanted

print '(/,A,A)', "Your original string is: ", inputString


select case(change_case(inputString,caseWanted))
  case(1)
  	print '(A)', "Error: enter either 'upper' or 'lower'"
  case default
    print '(/,A,A)', "Your new string is: ", inputString 	
  end select
   
end program test_upper