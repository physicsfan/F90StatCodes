module suffix_types
  
contains
  
  integer function add_suffix(file_name, suffix) result(answer)
    ! Returns 0 if operation is successful, 1 otherwise.
    implicit none
    character(len=*), intent(inout) :: file_name
    character(len=3), intent(in) :: suffix
    ! declare locals
    integer :: i
    
    if(suffix(1:1)=="") goto 5				! suffix begins with blank
    i = index(file_name, ".", back=.true.)              ! look for last period
    if(i==0) i = len_trim(file_name) + 1  	        ! if no period was found
    if(len(file_name) < i+3) goto 5			! not enough room for suffix
    
    file_name(i:) = "." // suffix	! add suffix to file name
    
    ! normal exit
    answer = 0
    return
    ! error trap
5   answer = 1
    return       
  end function add_suffix
  
end module suffix_types

program test_suffix
  use suffix_types
  ! a main program to test the add_suffix function
  implicit none
  
  integer, parameter :: fileLengthMax = 80
  integer, parameter :: suffixLength = 4
  character(len=fileLengthMax) :: fileName = ""
  character(len=suffixLength) :: suffix = ""
  
  ! section if multiple file-names are sought
  !integer :: number_of_filenames
  !character(len=fileLengthMax), allocatable :: fileNames(:)
  
  !print "(A)", "Please enter the number of filenames wanted:"
  !read(*,*) number_of_filenames
  
  !allocate(fileNames(number_of_filenames))
  
  
  print "(A)", "Please enter the name of the file and its desired suffix:"
  read(*,*) fileName, suffix
  
  if (add_suffix(fileName, suffix) == 0) then
     print "(A)", "Success!"
     print "(A,A)", "Your new file name is: ",fileName
  else
     print "(A)", "There was an error"
  endif
  
end program test_suffix
