! ###################################################################
module root

 contains
  integer function cube_root(x,y) result(answer)
     implicit none
     real, intent(in) :: x
     real, intent(out) :: y
     real :: old
     if(x < 0.0) then
         goto 5
     else   
     	 if(x == 0.0) then
         y = 0.0
     	 else
         y = 1.0
         do
            old = y
            y = (2.0*old + x/(old**2)) / 3.0
            if(y == old) exit
         end do
     end if
     ! normal exit
     answer = 0
     return
     ! error trap
5    answer = 1
   end function
   
! ###################################################################
  integer function square_root(x,y) result(answer)
     implicit none
     real, intent(in) :: x
     real, intent(out) :: y
     real :: old
     if(x < 0.0) then
         goto 6
     else if(x == 0.0) then
         y = 0.0
     else
         y = 1.0
         do
            old = y
            y = (old + x/old) / 2.0
            if(y == old) exit
         end do
     end if
     ! normal exit
     answer = 0
     return
     ! error trap
6    answer = 1
   end function   
end module root

! ###################################################################
program i_will_croot_you
  use root
! A short program that tests the cube_root() function
  implicit none
  real :: x, y
  integer :: cube_root
  read(*,*) x
  if(cube_root(x,y) > 0) then
      print "(A)", "I can't handle that!"
  else
      print *, y
  end if
end program i_will_croot_you
