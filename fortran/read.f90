module read

    interface read_file
        module procedure read_character
    end interface read_file

    contains

    subroutine read_dimensions(file_len,line_len,io)
        integer :: file_len,line_len,io
        
        read(io,*) file_len
        read(io,*) line_len
    end subroutine read_dimensions

    subroutine read_character(input,io)
        character(len=*) :: input(:)
        integer          :: i, io

        do i=1,size(input)
            read(io,*) input(i)
        end do
    end subroutine read_character

end module read