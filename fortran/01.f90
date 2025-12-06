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
        integer :: password_p1
        call read_file(input,io)
        close(io)

        call part_one(input,password_p1)
        print*,"the password is",password_p1
    end subroutine main

    subroutine part_one(input,password)
        character(len=line_len) :: input(file_len)
        character :: dirn
        integer :: password,dial,i,dist
        password = 0
        dial = 50
        do i=1,file_len
            dirn = input(i)(1:1)
            dist = string_to_int(trim(input(i)(2:)))
            select case (dirn)
                case ("L")
                    dial = dial - dist
                case ("R")
                    dial = dial + dist
                case default
                    print*,"whoops"
            end select
            dial = mod(dial,100)
            if (dial.lt.0) then
                dial = 100 + dial
            end if
            if (dial.eq.0) then
                password = password + 1
            end if
        end do
    end subroutine part_one

    subroutine part_two()

    end subroutine part_two

end program secret_entrance