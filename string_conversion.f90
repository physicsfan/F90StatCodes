module string_conversion
  ! converts a numerical argument into a string.
  private ! by default

  public :: convert_to_string

  interface convert_to_string
     ! calls apropriate procedure based on argument type
     module procedure real_to_string
     module procedure int_to_string
  end interface convert_to_string

contains

  subroutine int_to_string(arg, str)
    ! converts an integer argument into an equivalent string
    implicit none
    integer, intent(in) :: arg
    character(len=*), intent(out) :: str
    character(len=20) :: tmp   ! to prevent I/O errors
    write(tmp, *) arg
    str = adjustl(tmp)
  end subroutine int_to_string


  subroutine real_to_string(arg, str)
    ! converts a real argument into a string
    implicit none
    real, intent(in) :: arg
    character(len=*), intent(out) :: str
    character(len=20) :: tmp   ! to prevent I/O errors
    write(tmp, *) arg
    str = adjustl(tmp)
  end subroutine real_to_string


end module string_conversion
