! A simple program for testing tht error_handler module

program err_test
  use error_handler
  use precision
  use cholesky
  implicit none

  ! declare variables and parameters
  real(dp) :: s(3,3) = 0.d0
  integer :: i, j
  type(error_type) :: err
  character(len=*), parameter :: platform = "PC"
  character(len=256) :: msg_string

  ! enter a positive-definite matrix into lower triangle
  s(1,1) = 0.75 ; s(2,1) = -0.20 ; s(3,1) = -0.04
  s(2,2) = 4.66 ; s(3,2) = 1.43
  s(3,3) = 5.45

  ! attempt cholesky factorization and print result
  if(cholesky_lower(s, err) /= 0) goto 800
  do i = 1, 3
     print "(3F10.5)", (s(i,j), j=1, 3)
  end do

800 continue
  ! report error message
  if(err_msg_present(err)) then
     call err_get_msgs(err, msg_string, platform)
     print "(A)", trim(msg_string)
     print "(A)", "Aborted"
  end if

end program err_test
