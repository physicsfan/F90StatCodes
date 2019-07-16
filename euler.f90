! ###################################################################
module euler_module

 integer, parameter :: my_real = selected_real_kind(15,307)

 contains
  integer function euler_constant(eps, e) result(answer)
     implicit none
     ! declare variables
     real(kind=my_real), intent(in) :: eps
     real(kind=my_real), intent(out) :: e
     ! declare locals
     integer :: n = 1
     real(kind=my_real) :: a
     
     if(eps <= 0.0) then
         goto 5
     else   
         e=1.0
         a=1.0
         do
            a = a/n
            e = e + a
            n = n + 1
            if(a < eps) exit
         end do
     end if
     ! normal exit
     answer = 0
     return
     ! error trap
5    answer = 1
   end function
   
end module euler_module

! ###################################################################
program euler_test
  use euler_module
! A short program that tests the euler_module
  implicit none
  real(kind=my_real) :: epsilon, euler
  write(*,"(a)") "Enter the value of epsilon."
  read(*,*) epsilon
  if(euler_constant(epsilon,euler) > 0) then
      print "(A)", "epsilon must be greater than 0.0"
  else
      print "(/,A)", "Euler's constant e is"
      print "(f20.15)", euler
  end if
end program euler_test
