module euclid

!!$  Preamble:
!!$  This module contains tools for circumscribing a circle about a given triangle
!!$  using Euclid's method. The basic algorithm is the folowwing:
!!$  a) Choose any two sides of the given triange
!!$  b) Construct their perpendicular bisectors
!!$  c) Find the intersection point of the bisectors. This defines the center of the circle
!!$  d) Find the radius of the circle from the distance between one triangle vertex and the center

!!$  This module will eventually use the error_handler class 

  implicit none
  
  private                               ! by default

  ! set precision
  integer, parameter :: sp = kind(0.0)
  integer, parameter :: dp = kind(0.d0)
  real(sp), parameter :: pi = 4*atan(1.d0)

  ! explicit public types
  public :: sp
  public :: point_type
  public :: circle_type
  public :: triangle_type
  public :: line_type
  public :: print_shape
  public :: circumscribe_circle

  ! define derived types
  type :: point_type
     real(sp) :: x = 0.
     real(sp) :: y = 0.
  end type point_type

  type :: circle_type
     type(point_type) :: center
     real(sp) :: radius = 1.
  end type circle_type

  type :: triangle_type
     type(point_type) :: vertex1, vertex2, vertex3
  end type triangle_type

  type :: line_type
     real(sp) :: m = 1.
     real(sp) :: b = 0.
  end type line_type

  ! overload
    interface print_shape
     module procedure print_circle
     module procedure print_triangle
  end interface print_shape
  
contains

  subroutine print_circle(circle)
    implicit none
    ! declare arguments
    type(circle_type), intent(in) :: circle

    print "(/,A)", "Circle:"
    print *, "Center:", circle%center%x, circle%center%y
    print *, "Radius:", circle%radius

  end subroutine print_circle

  subroutine print_triangle(triangle)
    implicit none
    ! declare arguments
    type(triangle_type), intent(in) :: triangle

    print "(/,A)", "Triangle:"
    print *, "Vertex1:", triangle%vertex1%x, triangle%vertex1%y
    print *, "Vertex2:", triangle%vertex2%x, triangle%vertex2%y
    print *, "Vertex3:", triangle%vertex3%x, triangle%vertex3%y
    
  end subroutine print_triangle


  subroutine circumscribe_circle(triangle, circle)
    implicit none
    ! declare arguments
    type(triangle_type), intent(in) :: triangle
    type(circle_type), intent(out)  :: circle
    ! declare locals
    type(line_type)  :: bisector1, bisector2
    type(point_type) :: intersection
    integer :: error

    ! calculate two perpendicular bisectors
    call get_perpendicular_bisector(triangle%vertex1,triangle%vertex2,bisector1,error)
    call get_perpendicular_bisector(triangle%vertex2,triangle%vertex3,bisector2,error)

    ! calculate intersection point of bisectors
    call get_intersection_point(bisector1,bisector2,intersection)

    ! generate circumscribed circle
    circle%center = intersection
    circle%radius = sqrt((triangle%vertex1%x - intersection%x)**2 &
                         + (triangle%vertex1%y - intersection%y)**2)
  end subroutine circumscribe_circle

  subroutine get_perpendicular_bisector(point1, point2, bisector, error_code)
    implicit none
    ! declare arguments
    type(point_type), intent(in)   :: point1
    type(point_type), intent(in)   :: point2
    type(line_type), intent(out) :: bisector
    integer :: error_code
    ! declare locals
    type(point_type) :: midpoint
    real(sp) :: slope
    real(sp) :: zero = 0.0e-15

    ! calculate midpoint between point1 and point2
    midpoint%x = 0.5*(point1%x + point2%x)
    midpoint%y = 0.5*(point1%y + point2%y)

    ! calculate the slope from point1 and point2
    
    ! check if slope either 0 or infinite
    if((point2%y - point1%y) <= zero) then
       goto 100   
    else if((point2%x - point1%x) <= zero) then
       goto 200
    else
       slope = (point2%y - point1%y)/(point2%x - point1%x)
    end if
 
 ! calculate slope of bisector (m = -1/slope)
    bisector%m = -1./slope

    ! calculate y-intercept of bisector
    bisector%b = midpoint%y - bisector%m*midpoint%x
    error_code = 0
    return
    
    ! error trap
100 error_code = 1 ; return
200 error_code = 2 ; return
    
  end subroutine get_perpendicular_bisector

  subroutine get_intersection_point(line1, line2, int_point)
    implicit none
    ! declare arguments
    type(line_type), intent(in)   :: line1
    type(line_type), intent(in)   :: line2
    type(point_type), intent(out) :: int_point
    ! declare locals
    real(sp) :: determinant

    determinant = line1%m*line2%b - line1%b*line2%m    ! ad-bc

    int_point%x = (line2%b - line2%m) / (line1%m - line1%b)
    int_point%y = determinant / (line1%m - line1%b)
    
  end subroutine get_intersection_point

  
end module euclid



program euclid_test
  use euclid
  implicit none

  type(circle_type) :: circle
  type(triangle_type) :: triangle

  ! define triangle

  triangle%vertex1%x = 0.
  triangle%vertex1%y = 0.
  triangle%vertex2%x = 3.
  triangle%vertex2%y = 1.
  triangle%vertex3%x = -0.5
  triangle%vertex3%y = 2.

  ! print results
  print "(/,A,/)", "Initial triangle:"
  call print_shape(triangle)

  ! circumscribe circle around triangle using Euclid's method
  call circumscribe_circle(triangle, circle)

  ! print results after rotation
  print "(/,A)", "Circumscribed circle:"
  call print_shape(circle)

end program euclid_test
