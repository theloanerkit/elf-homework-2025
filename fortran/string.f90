module string

    contains

    function string_to_int(str)
        integer          :: string_to_int
        character(len=*) :: str
        integer          :: i,temp
        
        string_to_int = 0
        do i=1,len(str)
            string_to_int = string_to_int*10
            read(str(i:i),'(I1)') temp
            string_to_int = string_to_int + temp
        end do
    end function string_to_int

end module string