module math_constants
  ! use precision
  implicit none
  ! Define precision
    integer, parameter :: dp = selected_real_kind(15,307)
  ! integer, parameter :: dp = kind(1.d0)                  ! alternate form

  ! Geometry
  real(dp), parameter :: deg_per_rad = 57.295779513082320876798155_dp
  real(dp), parameter :: rad_per_deg = 0.017453292519943295769237_dp

  ! Euler's constant e
  real(dp), parameter :: e_value     = 2.718281828459045235360287_dp
  real(dp), parameter :: e_recip     = 0.3678794411714423215955238_dp
  real(dp), parameter :: log10_of_e  = 0.4342944819032518276511289_dp

  ! Euler-Mascheroni constant
  real(dp), parameter :: euler        = 0.577215664901532860606_dp
  real(dp), parameter :: euler_log    = -0.5495393129816448223_dp
  real(dp), parameter :: gamma        = 0.577215664901532860606512_dp
  real(dp), parameter :: gamma_log    = -0.5495393129816448223376612_dp

  ! Golden Ratio
  real(dp), parameter :: golden_ratio = 1.618033988749894848_dp

  ! Logarithms
  real(dp), parameter :: ln_2         = 0.6931471805599453094172321_dp
  real(dp), parameter :: ln_10        = 2.3025850929940456840179915_dp
  real(dp), parameter :: log10_of_2   = 0.3010299956639811952137389_dp

  ! Pi
  real(dp), parameter :: pi_value     = 3.141592653589793238462643_dp
  real(dp), parameter :: pi_ln        = 1.144729885849400174143427_dp
  real(dp), parameter :: pi_log10     = 0.4971498726941338543512683_dp
  real(dp), parameter :: pi_over_2    = 1.570796326794896619231322_dp
  real(dp), parameter :: pi_over_3    = 1.047197551196597746154214_dp
  real(dp), parameter :: pi_over_4    = 0.7853981633974483096156608_dp
  real(dp), parameter :: pi_recip     = 0.3183098861837906715377675_dp
  real(dp), parameter :: pi_squared   = 9.869604401089358618834491_dp
  real(dp), parameter :: pi_sq_root   = 1.772453850905516027298167_dp

  ! Roots
  real(dp), parameter :: sq_root_of_2 = 1.4142135623730950488_dp
  real(dp), parameter :: sq_root_of_3 = 1.7320508075688772935_dp

end module math_constants
  
!!$program math_test
!!$  use math_constants                   ! access all constants
!!$  real :: pi                           ! define local data type
!!$  print *, 'pi_value: ', pi_value      ! display a constant
!!$  pi = pi_value; print *, 'pi = ', pi  ! convert to lower precision
!!$end program math_test
