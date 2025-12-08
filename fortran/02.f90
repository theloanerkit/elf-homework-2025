program gift_shop
    use constants
    use read, only : read_dimensions, read_file
    use string, only : string_to, string_count_char
    implicit none
    integer :: file_len,line_len,io
    character(len=2) :: day = "02"
    io = 17
    open(newunit=io,file=day)
    call read_dimensions(file_len,line_len,io)

    call main()

    contains

    subroutine main()
        character(len=line_len) :: input_str(file_len)
        character(len=line_len),allocatable :: input(:)
        integer(kind=int_16) :: sum_of_invalid_ids

        call read_file(input_str,io)
        close(io)

        call string_to(input_str(1),input,",",len(input))

        call part_one(input,sum_of_invalid_ids,string_count_char(input_str(1),",")+1)
        print*,"the sum of invalid ids is ",sum_of_invalid_ids

        deallocate(input)
    end subroutine main

    subroutine part_one(input,sum_of_invalid_ids,l)
        character(len=line_len),allocatable :: input(:)
        integer(kind=int_16) :: sum_of_invalid_ids,c,a,b,start
        integer(kind=int_16), allocatable :: id_range(:)
        integer :: i,l, half_length, full_length_start,full_length_stop
        
        sum_of_invalid_ids = 0
        do i=1,l
            call string_to(input(i),id_range,"-")
            full_length_start = ceiling(log10(real(id_range(1))))
            full_length_stop = ceiling(log10(real(id_range(2))))
            half_length = full_length_start/2
            if (mod(full_length_start,2).eq.1.and.full_length_start.eq.full_length_stop) then
                ! both start and stop ID have same, odd length, so cannot be made of 2 repeated numbers
                deallocate(id_range)
                cycle
            else if (mod(full_length_start,2).eq.1) then
                ! starting on a number with an odd number of digits, but not finishing there
                ! invalid ID *must* have an even number of digits, so start at the next number that satisfies that
                start = 10**(half_length)
            else
                ! divide number into two halves, and start on the smallest
                a = id_range(1) - (id_range(1)/(10**half_length))*10**half_length
                b = id_range(1)/(10**half_length)
                start = min(a,b)
            end if
            c = concat(start)
            do while (c.le.id_range(2))
                if (c.ge.id_range(1).and.c.le.id_range(2)) then
                    ! invalid ID found
                    sum_of_invalid_ids = sum_of_invalid_ids + c
                end if
                start = start + 1
                c = concat(start)
            end do
            if (allocated(id_range)) then
                deallocate(id_range)
            end if
        end do
    end subroutine part_one

    function concat(num)
        integer(kind=int_16) :: num,concat
        integer :: l
        l = floor(log10(real(num))) + 1
        concat = num + (num*10**l)
    end function concat

    subroutine part_two()

    end subroutine part_two

end program gift_shop
