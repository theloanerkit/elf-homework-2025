program printing_department
    use read, only : read_dimensions, read_character_grid
    use grid, only : search_adjacent
    implicit none
    integer :: file_len,line_len,io
    character(len=2) :: day = "04"
    io = 23
    open(newunit=io,file=day)
    call read_dimensions(file_len,line_len,io)

    call main()

    contains

    subroutine main()
        character :: input(file_len,line_len)
        integer :: paper_rolls
        call read_character_grid(input,io)
        close(io)

        call part_one(input,paper_rolls)
        print*,"the forklifts can get to",paper_rolls,"rolls of paper"

    end subroutine main

    subroutine part_one(input,paper_rolls)
        character :: input(file_len,line_len)
        integer :: paper_rolls,i,j,adj
        paper_rolls = 0
        do i=1,size(input,dim=1)
            do j=1,size(input,dim=2)
                if (input(i,j).eq."@") then
                    adj = search_adjacent(input,"@",(/i,j/))
                    if (adj.lt.4) then
                        paper_rolls = paper_rolls + 1
                    end if
                end if
            end do
        end do
    end subroutine part_one

    subroutine part_two()

    end subroutine part_two

end program printing_department