module pi_monte_carlo
  
contains

  subroutine display_initial_header()
    ! display initial header
    write (*,'(/a/)') "PI Estimation Program"
  end subroutine display_initial_header


  
  subroutine error_handler(error_flag)
    implicit none
    integer :: error_flag

    select case (error_flag)
    case(1)
       write(*,'(a,/a)') "Too many incorrect attempts!", &
            "Program terminated."
       stop
    case(2)
       write(*,'(a,/a)') "Error, unable to allocate memory.", &
        "Program terminated."
       stop
       
    end select  
  end subroutine error_handler

  
  subroutine fill_with_uniforms(mat, lower, upper)
    ! Fills the rank_one array vec with random numbers uniformly
    ! distributed from lower to upper, which default to 0.0 and
    ! 1.0, respectively
    implicit none
    
    ! declare arguments
    real, intent(out) :: mat(:,:)
    real, intent(in), optional :: lower, upper
    
    ! declare locals
    real :: a, b
    
    ! begin
    a = 0.0
    b = 1.0
    if(present(lower)) a = lower
    if(present(upper)) b = upper
    call random_number(mat)
    mat = a + mat * (b - a)
    
  end subroutine fill_with_uniforms
  
  
  subroutine get_count_value(count)
    implicit none

    ! declare arguments
    integer :: count

    ! declare locals
    integer :: trycount
    
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
       
       if(trycount > 3) call error_handler(1)
      
    end do
  end subroutine get_count_value

  
  subroutine init_random_seed()
    ! subroutine to initialize the random number generator
    
    ! declare locals
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
    
    ! initialize random seed with system clock
    call random_seed(size = n)
    allocate(seed(n))
    call system_clock(count=clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(PUT = seed)
    
    deallocate(seed)
    
  end subroutine init_random_seed
  
  subroutine monte_carlo(point_mat, pi_est)
    implicit none
    ! declare variables
    real, intent(in) :: point_mat(:,:)
    real, intent(out) :: pi_est

    ! declare locals
    integer :: count, i, incount
    real :: pt
    
    ! perform monte carlo estimation
    count = size(point_mat,1)
    
    ! set count of samples inside circle = 0
    incount = 0
    ! loop count times
    do i = 1, count
       
       pt = point_mat(i,1)**2 + point_mat(i,2)**2
       if(sqrt(pt) < 1.0) incount = incount + 1
       
    end do
    
    pi_est = 4.0 * real(incount) / real(count)
  end subroutine monte_carlo

  subroutine print_results(count, pi_est)
    implicit none
    integer :: count
    real :: pi_est
    write(*,'(/a, i7)') "Count of points: ", count
    write(*,'(a, f8.6)') "Estimated PI value: ", pi_est
  end subroutine print_results
  
end module pi_monte_carlo


program piestimation 
use pi_monte_carlo
! this program uses a Monte Carlo routine to generate an estimate of pi based
! on a number of steps specified by the user (100-1,000,000 steps)

! declare variables
implicit none
integer :: count, alstat, i, incount, j
real :: x, y, piEst, pt
real, allocatable, dimension(:,:) :: points    ! define a dynamic array

call display_initial_header

! prompt for and obtain count value
call get_count_value(count)

! allocate 2D array
allocate(points(count,2), stat=alstat)
if(alstat /= 0) call error_handler(2)

call init_random_seed()	! initialize random number generator

! fill array with random points
call fill_with_uniforms(points)

call monte_carlo(points, piEst)


! display results
call print_results(count,piEst)

end program piestimation
