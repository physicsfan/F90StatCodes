program binomial
! This program uses allocatable arrays to compute and print the 
! binomial probability mass function:
!	
!			f(y) = (n)p^y *(1 - p)^(n-y),		y=0,1,...,n
!			       (y)
!
! and the cumulative distribution:
!
!			F(y) = f(0) + f(1) + ... + f(y)
!
! for user-specific values of n and p.
  implicit none
  integer(kind=3) :: i, n, n_factorial, y, y_factorial, y_minus_n, y_minus_n_factorial
  real*8 :: p
  real*8, allocatable :: f(:), cdF(:)

! read in values  
  print "(A)", "Enter the value of p: (0 <= p <= 1)"
  read(*,*) p
  print "(A)", "Enter the value of n:"
  read(*,*) n
  
  allocate(f(0:n))
  allocate(cdF(0:n))

! print *, lbound(f), ubound(f)
! print *, lbound(cdF), ubound(cdF)
  
  f=0.0
  cdF=0.0

  print "(A13,A15,A15)", "y","f(y)","F(y)"

! calculate n-factorial
  n_factorial=1
  do i=1,n
    n_factorial=n_factorial*i
  end do
  
  do y=0,n
! calculate y-factorial
  	y_factorial=1
  	do i=1,y
      y_factorial=y_factorial*i
  	end do

! calcualte (n-y)-factorial
  	y_minus_n=y-n
  	y_minus_n_factorial=1
  	do i=1,y_minus_n
    	y_minus_n_factorial=y_minus_n_factorial*i
  	end do

! calculate f(y) and cdF(y)
    f(y)=n_factorial * (p**y * (1.0d0-p)**y_minus_n) / (y_factorial*y_minus_n_factorial)
    cdF(y) = sum(f)

    print "(I13,f25.13,f25.12)", y, f(y), cdF(y)
    
  end do

  deallocate(f, cdF)   ! not really necessary

end program
