program pair_of_dice
    ! Generates random numbers uniformly distributed between a and b
    ! Version 1
    implicit none

    integer :: i, n, die1, die2, face_sum(2:12) = 0
    real :: min_face, max_face, u

    ! initialize random_seed
        integer :: values(1:8), k
        integer, dimension(:), allocatable :: seed
        call date_and_time(values=values)
        call random_seed(size=k)
        allocate(seed(1:k))
        seed(:) = values(8)
        call random_seed(put=seed)

    ! assume 6 sided dice
    min_face = 1.0
    max_face = 7.0                 ! needed for uniform face probabilities since the int function rounds down

    print "(A)", "How many throws do you want?"
    read(*,*) n
    print "(A)", "Here are the first 10:"

    do i = 1, n
        call random_number(u)
        die1 = int(min_face + u*(max_face-min_face))
        if(die1 == 7) die1 = 6            ! should introduce an error of only 0.000001 in probability 1/6

        call random_number(u)
        die2 = int(min_face + u*(max_face-min_face))
        if(die2 == 7) die2 = 6

        if(i <= 10) print *, die1 + die2

        select case(die1+die2)
            case(2)
                face_sum(2) = face_sum(2)+1
            case(3)
                face_sum(3) = face_sum(3)+1
            case(4)
                face_sum(4) = face_sum(4)+1
            case(5)
                face_sum(5) = face_sum(5)+1
            case(6)
                face_sum(6) = face_sum(6)+1
            case(7)
                face_sum(7) = face_sum(7)+1
            case(8)
                face_sum(8) = face_sum(8)+1
            case(9)
                face_sum(9) = face_sum(9)+1
            case(10)
                face_sum(10) = face_sum(10)+1
            case(11)
                face_sum(11) = face_sum(11)+1
            case(12)
                face_sum(12) = face_sum(12)+1
        end select
    end do

    ! print probabilities
    if(n >= 1000) then
        print *, "2 appears with probability:", real(face_sum(2))/real(n)
        print *, "3 appears with probability:", real(face_sum(3))/real(n)
        print *, "4 appears with probability:", real(face_sum(4))/real(n)
        print *, "5 appears with probability:", real(face_sum(5))/real(n)
        print *, "6 appears with probability:", real(face_sum(6))/real(n)
        print *, "7 appears with probability:", real(face_sum(7))/real(n)
        print *, "8 appears with probability:", real(face_sum(8))/real(n)
        print *, "9 appears with probability:", real(face_sum(9))/real(n)
        print *, "10 appears with probability:", real(face_sum(10))/real(n)
        print *, "11 appears with probability:", real(face_sum(11))/real(n)
        print *, "12 appears with probability:", real(face_sum(12))/real(n)
    end if

end program pair_of_dice
