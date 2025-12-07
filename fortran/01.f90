program secret_entrance
    use read, only : read_dimensions, read_file
    use string, only : string_to_int
    implicit none
    integer :: file_len,line_len,io
    character(len=2) :: day = "01"
    io = 13
    open(newunit=io,file=day)
    call read_dimensions(file_len,line_len,io)

    call main()

    contains

    subroutine main()
        character(len=line_len) :: input(file_len)
        integer :: password_p1,password_p2
        call read_file(input,io)
        close(io)

        call part_one_and_two(input,password_p1,password_p2)
        print*,"the password is",password_p1
        print*,"the actual password is",password_p2
    end subroutine main

    subroutine part_one_and_two(input,password_p1,password_p2)
        character(len=line_len) :: input(file_len)
        character :: dirn
        integer :: password_p1,password_p2,dial,i,dist,temp
        password_p1 = 0
        password_p2 = 0
        dial = 50
        do i=1,file_len
            temp = dial
            dirn = input(i)(1:1)
            dist = string_to_int(trim(input(i)(2:)))
            select case (dirn)
                case ("L")
                    dial = dial - dist
                case ("R")
                    dial = dial + dist
                case default
                    ! something has gone wrong with the input if we make it here
                    print*,"whoops"
            end select

            do while (dial.lt.0)
                if (temp.ne.0) then
                    ! don't double count if we start on zero
                    password_p2 = password_p2+1
                end if
                temp=dial
                dial = dial + 100
            end do
            do while (dial.gt.99)
                ! passed 99, so need to reset dial
                if (dial.gt.100) then
                    ! otherwise we landed exactly on 100, which will get counted by if statement below
                    password_p2 = password_p2+1
                end if
                dial=dial-100
            end do

            if (dial.eq.0) then
                ! landed on zero
                password_p1 = password_p1+1
                password_p2 = password_p2+1
            end if
        end do
    end subroutine part_one_and_two

end program secret_entrance
