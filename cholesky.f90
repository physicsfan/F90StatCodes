module cholesky
  use error_handler
  use precision
  implicit none

  private    ! by default

  public :: cholesky_lower
  character(len=*), parameter :: modname = "cholesky"

contains
  integer function cholesky_lower(a, err) result(answer)
    ! Overwrites lower triangle of a symmetric, positive-definite matrix
    ! with its cholesky factor. The upper triangle is untouched.
    ! Returns 0 is successful, 1 if failed
    implicit none

    ! declare arguments
    real(dp), intent(inout) :: a(:,:)
    type(error_type), intent(inout) :: err

    ! declare local variables and parameters
    character(len=*), parameter :: subname = "cholesky_lower"
    integer :: p, i, j, k
    real(dp) :: sum

    !begin
    answer = 1
    p = size(a,1)
    if(p /= size(a,2)) goto 700
    do j = 1, p
       sum = 0.d0
       do k = 1, j-1
          sum = sum + a(j,k)**2
       end do
       if(sum >= a(j,j)) goto 710
       a(j,j) = sqrt(a(j,j) - sum)
       do i = j+1, p
          sum = 0.d0
          do k = 1, j-1
             sum = sum + a(j,k)*a(i,k)
          end do
          if(a(j,j) == 0.d0) goto 710
          a(i,j) = (a(i,j) - sum) / a(j,j)
       end do
    end do

    ! normal exit
    answer = 0
    return

    ! error traps
700 call err_handle(err, 300, called_from = subname//" in MOD "//modname)
    return
710 call err_handle(err, 101, called_from = subname//" in MOD "//modname)
    return
    
  end function cholesky_lower

end module cholesky
