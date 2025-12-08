module grid
    implicit none

    interface search_adjacent ! looks at all 8 tiles -> search_orthogonal does 4
        module procedure search_adjacent_char
    end interface search_adjacent

    contains

    function search_adjacent_char(grid,char,idx)
        character :: grid(:,:),char
        integer :: search_adjacent_char
        integer :: idx(2),i,j,tmp_idx(2)
        search_adjacent_char = 0
        do i=-1,1
            do j=-1,1
                tmp_idx(1) = idx(1) + i
                tmp_idx(2) = idx(2) + j
                if (all(tmp_idx.eq.idx)) cycle
                if (tmp_idx(1).lt.1.or.tmp_idx(1).gt.size(grid,dim=1)) cycle
                if (tmp_idx(2).lt.1.or.tmp_idx(2).gt.size(grid,dim=2)) cycle
                if (grid(tmp_idx(1),tmp_idx(2)) .eq. char) then
                    search_adjacent_char = search_adjacent_char + 1
                end if
            end do
        end do

    end function search_adjacent_char

end module grid