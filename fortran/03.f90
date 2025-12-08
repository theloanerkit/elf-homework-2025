program lobby
    use read, only : read_dimensions, read_file
    use string, only : string_to
    implicit none
    integer :: file_len,line_len,io
    character(len=2) :: day = "03"
    io = 19
    open(newunit=io,file=day)
    call read_dimensions(file_len,line_len,io)

    call main()

    contains

    subroutine main()
        character(len=line_len) :: input(file_len)
        integer :: output_joltage
        call read_file(input,io)
        close(io)

        call part_one(input,output_joltage)
        print*,"the total output joltage is",output_joltage
    end subroutine main

    subroutine part_one(input,output_joltage)
        character(len=line_len) :: input(file_len)
        integer :: output_joltage,i
        integer,allocatable :: battery_bank(:)
        output_joltage = 0
        do i=1,file_len ! iterate over battery banks
            call string_to(input(i),battery_bank)
            output_joltage = output_joltage + maximum_joltage(battery_bank,2)
            deallocate(battery_bank)
        end do
    end subroutine part_one

    function maximum_joltage(battery_bank,battery_count)
        integer,allocatable :: battery_bank(:)
        integer :: battery_count,maximum_joltage,i,idx(1),tmp,on,start
        maximum_joltage = 0
        on = 0
        start = 0
        do i = 1,battery_count ! how many batteries do we need to turn on
            idx = maxloc(battery_bank(start+1:)) ! find maximum energy battery *after* the one we just found
            tmp = idx(1) + start
            do while (size(battery_bank)-tmp.lt.battery_count-on-1)
                ! if we found a battery too close to the end to fit the rest of our batteries in
                idx = maxloc(battery_bank(start+1:tmp-1))
                tmp = idx(1) + start
            end do
            maximum_joltage = maximum_joltage*10
            maximum_joltage = maximum_joltage + battery_bank(tmp)
            start = idx(1)
            on = on + 1
        end do
    end function maximum_joltage

    subroutine part_two()

    end subroutine part_two

end program lobby