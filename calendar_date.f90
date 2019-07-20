module calendar
  implicit none

  public

  integer, parameter :: month_string_length = 10
  integer, parameter :: date_string_length = 19

  type :: calendar_date_type
     character(len=month_string_length) :: month = "January"
     integer :: day = 1, year = 2000
  end type calendar_date_type

  contains

    subroutine get_current_date(calendar_date)
      implicit none
      ! declare arguments
      type(calendar_date_type), intent(out) :: calendar_date
      ! declare locals
      integer :: date_time(8)
      character(len=8)  :: date     ! date in CCYYMMDD
      character(len=10) :: time     ! current time in hhmmss.sss
      character(len=5)  :: zone     ! time difference wrt UTC in hhmm

      ! obtain current date using function "date_and_time"
      call date_and_time(date,time,zone,date_time)

      ! populate calendar_date
      calendar_date%year = date_time(1)
      calendar_date%day  = date_time(3)

      select case(date_time(2))
      case(1)
         calendar_date%month = "January"
      case(2)
         calendar_date%month = "February"
      case(3)
         calendar_date%month = "March"
      case(4)
         calendar_date%month = "April"
      case(5)
         calendar_date%month = "May"
      case(6)
         calendar_date%month = "June"
      case(7)
         calendar_date%month = "July"
      case(8)
         calendar_date%month = "August"
      case(9)
         calendar_date%month = "September"
      case(10)
         calendar_date%month = "October"
      case(11)
         calendar_date%month = "November"
      case(12)
         calendar_date%month = "December"
      end select
      
    end subroutine get_current_date

    
    subroutine output_date_string(calendar_date)
      ! This prints the contents of calendar_date in a left-
      ! justified character string. This will be accomplished
      ! using internal files.
      implicit none
      ! declare arguments
      type(calendar_date_type), intent(in) :: calendar_date
      ! declare locals
      character(len=date_string_length) :: date_string
      character(len=2) :: sDay
      character(len=4) :: sYear

      ! print day and year to internal files to convert to strings
      write(sDay, "(I2)") calendar_date%day
      write(sYear,"(I4)") calendar_date%year

      date_string = adjustr(calendar_date%month) // " " // adjustl(sDay) // ", " // adjustr(sYear)

      ! output date_string
      write(*,"(A)") adjustl(date_string)
      
    end subroutine output_date_string


  end module calendar

!#############################################################################
  
  program calendar_test
    ! This program tests the module calendar
    use calendar
    implicit none

    type(calendar_date_type) :: date

    
    call get_current_date(date)

    call output_date_string(date)

  end program calendar_test
    
