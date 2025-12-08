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
        integer(kind=int_16) :: sum_of_invalid_ids_p1,sum_of_invalid_ids_p2

        call read_file(input_str,io)
        close(io)

        call string_to(input_str(1),input,",",len(input))

        call part_one(input,sum_of_invalid_ids_p1,string_count_char(input_str(1),",")+1)
        print*,"the sum of invalid ids is ",sum_of_invalid_ids_p1

        ! longest ID is 10 digits long
        ! length = 1
            ! no invalid IDS
        ! length = 2
            ! x + x
        ! length = 3
            ! x + x + x
        ! length = 4
            ! x + x + x + x
            ! xx + xx
        ! length = 5
            ! x + x + x + x + x
        ! length = 6
            ! x + x + x + x + x + x
            ! xx + xx + xx
            ! xxx + xxx
        ! length = 7
            ! x + x + x + x + x + x + x
        ! length = 8
            ! x + x + x + x + x + x + x + x
            ! xx + xx + xx + xx
            ! xxxx + xxxx
        ! length = 9
            ! x + x + x + x + x + x + x + x + x
            ! xxx + xxx + xxx
        ! length = 10
            ! x + x + x + x + x + x + x + x + x + x
            ! xx + xx + xx + xx + xx
            ! xxxxx + xxxxx
        call part_two(input,sum_of_invalid_ids_p2,string_count_char(input_str(1),",")+1)
        print*,"the actual sum of invalid ids is ",sum_of_invalid_ids_p2

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

    function concat_to_length(num,length)
        integer(kind=int_16) :: num,concat_to_length
        integer :: l,length,temp
        concat_to_length = num
        temp = ceiling(log10(real(concat_to_length)))
        l = floor(log10(real(concat_to_length))) + 1
        !print*,"in concat to length"
        do while (temp.lt.length)
            !print*,concat_to_length,10**l
            concat_to_length = concat_to_length*10**l
            concat_to_length = concat_to_length + num
            temp = ceiling(log10(real(concat_to_length)))
        end do
        !print*,"outr"
    end function concat_to_length

    subroutine part_two(input,sum_of_invalid_ids,l)
        character(len=line_len),allocatable :: input(:)
        integer(kind=int_16) :: sum_of_invalid_ids,test,j,tmp_int
        integer(kind=int_16), allocatable :: id_range(:),found(:),tmp_list(:)
        integer :: l,i, full_length_start,full_length_stop
        integer :: repeats,k,idx

        sum_of_invalid_ids = 0

        do i=1,l
            call string_to(input(i),id_range,"-")
            full_length_start = ceiling(log10(real(id_range(1))))
            full_length_stop = ceiling(log10(real(id_range(2))))

            if (full_length_start.eq.1.and.full_length_stop.eq.1) then
                ! if ID is in the range 1-9 it is always valid
                deallocate(id_range)
                cycle
            end if
            allocate(found(20),source=0_int_16)
            idx = 1
            ! all ranges of IDs have the option to be made up of 1 digit patterns
            if (full_length_start.ne.1) then
                do j=1,9
                    test = concat_to_length(j,full_length_start)
                    if (test.ge.id_range(1).and.test.le.id_range(2)) then
                        found(idx) = test
                        idx = idx + 1
                        sum_of_invalid_ids = sum_of_invalid_ids + test
                    end if
                end do
            end if
            if (full_length_stop.ne.full_length_start) then
                do j=1,9
                    test = concat_to_length(j,full_length_stop)
                    if (test.ge.id_range(1).and.test.le.id_range(2)) then
                        if (.not.any(found==test)) then
                            sum_of_invalid_ids = sum_of_invalid_ids + test
                            if (idx.gt.size(found)) then
                                allocate(tmp_list(size(found)+10),source=0_int_16)
                                tmp_list(1:size(found)) = found
                                deallocate(found)
                                allocate(found(size(tmp_list)),source=0_int_16)
                                found = tmp_list
                                deallocate(tmp_list)
                            end if
                            found(idx) = test
                            idx = idx + 1
                        end if
                    end if
                end do
            end if
            
            do k=2,5
                repeats = full_length_start/k
                tmp_int = 10**(k-1) 
                if (mod(full_length_start,k).eq.0.or.mod(full_length_stop,k).eq.0) then
                    if (mod(full_length_start,k).eq.0.and.full_length_start/k.gt.1) then
                        test = concat_to_length(tmp_int,full_length_start)
                        do while (test.le.id_range(2))
                            if (test.ge.id_range(1).and.test.le.id_range(2)) then
                                if (.not.any(found==test)) then
                                    sum_of_invalid_ids = sum_of_invalid_ids + test
                                    if (idx.gt.size(found)) then
                                        allocate(tmp_list(size(found)+10),source=0_int_16)
                                        tmp_list(1:size(found)) = found
                                        deallocate(found)
                                        allocate(found(size(tmp_list)),source=0_int_16)
                                        found = tmp_list
                                        deallocate(tmp_list)
                                    end if
                                    found(idx) = test
                                    idx = idx + 1
                                end if
                            end if
                            tmp_int = tmp_int + 1
                            test = concat_to_length(tmp_int,full_length_start)
                        end do
                    else if (mod(full_length_stop,k).eq.0.and.full_length_stop/k.gt.1) then
                        test = concat_to_length(tmp_int,full_length_stop)
                        do while (test.le.id_range(2))
                            if (test.ge.id_range(1).and.test.le.id_range(2)) then
                                if (.not.any(found==test)) then
                                    sum_of_invalid_ids = sum_of_invalid_ids + test
                                    if (idx.gt.size(found)) then
                                        allocate(tmp_list(size(found)+10),source=0_int_16)
                                        tmp_list(1:size(found)) = found
                                        deallocate(found)
                                        allocate(found(size(tmp_list)),source=0_int_16)
                                        found = tmp_list
                                        deallocate(tmp_list)
                                    end if
                                    found(idx) = test
                                    idx = idx + 1
                                end if
                            end if
                            tmp_int = tmp_int + 1
                            test = concat_to_length(tmp_int,full_length_start)
                        end do
                    end if
                end if  
            end do

            if (allocated(id_range)) then
                deallocate(id_range)
            end if
            deallocate(found)
        end do
    end subroutine part_two

end program gift_shop
