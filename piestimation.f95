program Piestimation

! this program uses a Monte Carlo routine to generate an estimate of pi based
! on a number of steps specified by the user (100-1,000,000 steps)

! declare variables
implicit none
integer :: count, alstat, i, incount, j, trycount
real :: x, y, pi_est, pt
real, allocatable, dimension(:,:) :: points    ! define a dynamic array

! display initial header
write (*,'(/a/)') "PI Estimation Program"

! prompt for and obtain count value
trycount = 0
do
   ! prompt for count value
   write(*,'(a)', advance="no") "Enter Count (100-1,000,000): "
   
   ! read count value
   read(*,*) count

   ! is count is correct, exit loop
   if(count >= 100 .and. count <= 1000000) exit

   ! display error message
   write (*,'(a,a,/a)') "Error, count must be ",   &
        "between 100 and 1,000,000.",              &
        "Please re-enter."
   
   trycount = trycount + 1

   if(trycount > 3) then
   write(*,'(a,/a)') "Too many incorrect attempts!", &
        "Program terminated."
   stop
end if

end do

! allocate 2D array
allocate(points(count,2), stat=alstat)
if(alstat /= 0) then
   write(*,'(a,/a)') "Error, unable to allocate memory.", &
        "Program terminated."
   stop
end if

! display 10 iterations
do j = 1, 10

   ! generate points
   call random_seed()

   ! loop count times
   do i = 1, (count/10)*j
   
      ! generate x and y values
      call random_number(x)
      call random_number(y)
      
      ! place (x,y) values in array
      points(i,1) = x
      points(i,2) = y

   end do

   ! perform monte carlo estimation
   ! set count of samples inside circle = 0
   incount = 0


   ! loop count times
   do i = 1, (count/10)*j
      
      pt = points(i,1)**2 + points(i,2)**2
      if(sqrt(pt) < 1.0) incount = incount + 1
      
   end do

   pi_est = 4.0 * real(incount) / real(j*count/10)

   ! display results
   write(*,'(/a, i7)') "Count of points: ", (count/10)*j
   write(*,'(a, f8.6)') "Estimated PI value: ", pi_est
end do

end program Piestimation
