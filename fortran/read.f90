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
            read(io,'(A)') input(i)
        end do
    end subroutine read_character

    subroutine read_character_grid(input,io)
        character :: input(:,:)
        character(len=(size(input(1,:)))) :: str
        integer :: i, j, io
        do i=1,size(input,dim=1)
            read(io,'(A)') str
            do j=1,size(input,dim=2)
                input(i,j) = str(j:j)
            end do
        end do
    end subroutine read_character_grid

end module read