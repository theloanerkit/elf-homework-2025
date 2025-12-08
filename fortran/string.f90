module string
    use constants
    implicit none
    character(len=1) :: digits(10) = (/"0","1","2","3","4","5","6","7","8","9"/)

    interface string_to
        module procedure string_to_int
        module procedure string_to_int_16
        module procedure string_to_list
        module procedure string_to_int_list
        module procedure string_to_int_list_16
    end interface string_to

    contains

    subroutine string_to_int(str,int)
        integer          :: int
        character(len=*) :: str
        character :: char
        integer          :: i,temp(1)

        int = 0
        do i=1,len(str)
            temp = 0
            char = str(i:i)
            if (char.ne." ") then
                int = int*10
                temp = findloc(digits,char)
                int = int + temp(1)-1
            else
                exit
            end if
        end do
    end subroutine string_to_int

    subroutine string_to_int_16(str,int)
        integer(kind=int_16)          :: int
        character(len=*) :: str
        character :: char
        integer :: i
        integer(kind=int_16)          :: temp(1)

        int = 0
        do i=1,len(str)
            temp = 0
            char = str(i:i)
            if (char.ne." ") then
                int = int*10
                temp = findloc(digits,char)
                int = int + temp(1)-1
            else
                exit
            end if
        end do
    end subroutine string_to_int_16

    subroutine string_to_list(str,list,sep,ls)
        integer :: ls
        character                    :: sep
        character(len=ls)             :: str
        character(len=ls)      :: tmp
        character(len=ls),allocatable :: list(:)
        integer                      :: elem_count,i,j,k

        elem_count = string_count_char(str,sep) + 1
        allocate(character(len=ls) :: list(ls))
        k=1
        do i=1,elem_count
            j=1
            tmp = ""
            do while (str(k:k).ne.sep.and.k.le.len(str))
                tmp(j:j) = str(k:k)
                k = k+1
                j = j+1
                if (k.gt.len(str)) then
                    exit
                end if
            end do
            if (k.lt.len(str)) then
                k = k+1
            end if
            list(i) = tmp
        end do
    end subroutine string_to_list

    subroutine string_to_int_list(str,list,sep)
        character :: sep
        character(len=*) :: str
        character(len=len(str)),allocatable :: tmp(:)
        integer, allocatable :: list(:)
        integer :: elem_count,i,tmp_int,space_idx

        elem_count = string_count_char(str,sep) + 1
        allocate(list(elem_count),source=0)
        call string_to_list(str,tmp,sep,len(tmp))
        do i=1,elem_count
            space_idx=index(tmp(i)," ")-1
            call string_to_int(tmp(i)(1:space_idx),tmp_int)
            list(i) = tmp_int
        end do
        deallocate(tmp)        
    end subroutine string_to_int_list

    subroutine string_to_int_list_16(str,list,sep)
        character :: sep
        character(len=*) :: str
        character(len=len(str)),allocatable :: tmp(:)
        integer(kind=int_16), allocatable :: list(:)
        integer(kind=int_16) :: tmp_int
        integer :: elem_count,i,space_idx

        elem_count = string_count_char(str,sep) + 1
        allocate(list(elem_count),source=0_int_16)
        call string_to_list(str,tmp,sep,len(tmp))
        do i=1,elem_count
            space_idx=index(tmp(i)," ")-1
            call string_to_int_16(tmp(i)(1:space_idx),tmp_int)
            list(i) = tmp_int
        end do
        deallocate(tmp)        
    end subroutine string_to_int_list_16

    function string_count_char(str,char)
        integer          :: string_count_char
        character(len=*) :: str
        character        :: char
        integer          :: i

        string_count_char = 0
        do i=1,len(str)
            if (str(i:i).eq.char) then
                string_count_char = string_count_char+1
            end if
        end do
    end function string_count_char


end module string