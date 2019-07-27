module rotate_shapes
  implicit none

  private                               ! by default

  integer, parameter :: sp = kind(0.0)
  integer, parameter :: dp = kind(0.d0)
  real(sp), parameter :: pi = 4*atan(1.d0)

  ! explicit public types
  public :: sp
  public :: point_type
  public :: circle_type
  public :: triangle_type
  public :: rotate
  public :: print_shape

  ! define derived types
  type :: point_type
     real(sp) :: x
     real(sp) :: y
  end type point_type

  type :: circle_type
     type(point_type) :: center
     real(sp) :: radius
  end type circle_type

  type :: triangle_type
     type(point_type) :: vertex1, vertex2, vertex3
  end type triangle_type

  ! overload rotation subroutines
  interface rotate
     module procedure rotate_circle
     module procedure rotate_triangle
  end interface rotate

  ! overload print subroutines
  interface print_shape
     module procedure print_circle
     module procedure print_triangle
  end interface print_shape
  
contains

  subroutine rotate_circle(circle, pivot, angle)
    implicit none
    ! declare arguments
    type(circle_type), intent(out) :: circle
    type(point_type), intent(in) :: pivot
    real(sp), optional, intent(in) :: angle
    ! declare locals
    real(sp) :: theta
    real(sp) :: ccx,ccy

    theta = 0.

    if(present(angle)) theta = angle*pi/180.

    ccx = circle%center%x
    ccy = circle%center%y
    
    ! rotate x
    circle%center%x = pivot%x + (ccx-pivot%x)*cos(theta) - (ccy-pivot%y)*sin(theta)

    ! rotate y
    circle%center%y = pivot%y + (ccx-pivot%x)*sin(theta) + (ccy-pivot%y)*cos(theta)
    
  end subroutine rotate_circle
  

  subroutine rotate_triangle(triangle, pivot, angle)
    implicit none
    ! declare arguments
    type(triangle_type), intent(out) :: triangle
    type(point_type), intent(in) :: pivot
    real(sp), optional, intent(in) :: angle
    ! declare locals
    real(sp) :: theta
    real(sp) :: tv1x,tv2x,tv3x,tv1y,tv2y,tv3y

    theta = 0.

    if(present(angle)) theta = angle*pi/180.

    tv1x = triangle%vertex1%x
    tv2x = triangle%vertex2%x
    tv3x = triangle%vertex3%x

    tv1y = triangle%vertex1%y
    tv2y = triangle%vertex2%y
    tv3y = triangle%vertex3%y
    
    ! rotate x
    triangle%vertex1%x = pivot%x + (tv1x-pivot%x)*cos(theta) - (tv1y-pivot%y)*sin(theta)
    triangle%vertex2%x = pivot%x + (tv2x-pivot%x)*cos(theta) - (tv2y-pivot%y)*sin(theta)
    triangle%vertex3%x = pivot%x + (tv3x-pivot%x)*cos(theta) - (tv3y-pivot%y)*sin(theta)

    ! rotate y
    triangle%vertex1%y = pivot%y + (tv1x-pivot%x)*sin(theta) + (tv1y-pivot%y)*cos(theta)
    triangle%vertex2%y = pivot%y + (tv2x-pivot%x)*sin(theta) + (tv2y-pivot%y)*cos(theta)
    triangle%vertex3%y = pivot%y + (tv3x-pivot%x)*sin(theta) + (tv3y-pivot%y)*cos(theta)
    
  end subroutine rotate_triangle

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
  
end module rotate_shapes



program rotate_test
  use rotate_shapes
  implicit none

  type(circle_type) :: circle
  type(point_type) :: pivot
  type(triangle_type) :: triangle
  real(sp) :: angle

  ! define circle and triangle
  circle%center%x = 0.
  circle%center%y = 0.
  circle%radius   = 2.

  triangle%vertex1%x = 0.
  triangle%vertex1%y = 0.
  triangle%vertex2%x = 2.
  triangle%vertex2%y = 0.
  triangle%vertex3%x = 1.
  triangle%vertex3%y = 1.

  ! define pivot and angle
  pivot%x = 2.
  pivot%y = 2.
  angle   = 45.

  ! print results before rotation
  print "(/,A,/)", "Before rotation:"
  call print_shape(circle)
  call print_shape(triangle)

  ! perform rotation
  call rotate(circle, pivot, angle)
  call rotate(triangle, pivot, angle)

  ! print results after rotation
  print "(/,A)", "After rotation:"
  print "(A8,f5.2,A12,f5.2)", "pivotx = ",pivot%x," pivoty = ",pivot%y
  print "(A7,f6.2)", "angle =",angle
  call print_shape(circle)
  call print_shape(triangle)

end program rotate_test
