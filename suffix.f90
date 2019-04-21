module suffix

	contains

    integer function add_suffix(file_name, suffix) result(answer)
    	! Returns 0 if operation is successful, 1 otherwise.
        implicit none
        character(len=*), intent(inout) :: file_name
        character(len=3), intent(in) :: suffix
        integer :: i
        
		if(suffix(1:1)==::) goto 5				! suffix begins with blank
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
	end function add_suffix

    integer function fuck_suffix(file_name) result(answer)
    	! Adds a .fuck suffix to any filename
    	! Returns 0 if operation is successful, 1 otherwise.
        implicit none
        character(len=*), intent(inout) :: file_name
        character(len=4), intent(out) :: suffix = "fuck"
        integer :: i
        
		if(suffix(1:1)==::) goto 5				! suffix begins with blank
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
	end function fuck_suffix

end module suffix

program test_suffix
use suffix
! a main program to test the add_suffix function
implicit none