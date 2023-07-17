module game
  implicit none

  integer, parameter :: board_size_cl = 3
  integer, parameter :: strike_size_cl = 3

  enum, bind(C)
     enumerator :: CELL_EMPTY = 0
     enumerator :: CELL_CROSS
     enumerator :: CELL_KNOTT
  end enum

  ! TODO: Rename TLine to Ray
  type :: TLine
     integer, dimension(2) :: p, d
  end type TLine

contains
  function board_full(board) result(ok)
    integer, intent(in) :: board(board_size_cl,board_size_cl)
    logical :: ok

    integer :: x, y

    ok = .true.
    do x=1,board_size_cl
       do y=1,board_size_cl
          ok = board(x, y) /= 0
          if (.not. ok) return
       end do
    end do
  end function board_full

  pure function in_bounds(p) result(yes)
    integer, intent(in) :: p(2)
    logical :: yes
    yes = 1 <= p(1) .and. p(1) <= board_size_cl &
         .and. 1 <= p(2) .and. p(2) <= board_size_cl
  end function in_bounds

  function player_won(board, player, line) result(ok)
    integer,     intent(in)  :: board(board_size_cl, board_size_cl)
    integer,     intent(in)  :: player
    type(TLine), intent(out) :: line
    logical :: ok
    logical :: visited(board_size_cl, board_size_cl)
    integer :: p(2)

    integer :: x, y, dx, dy

    visited(:,:) = .false.

    ok = .false.
    do x=1,board_size_cl
       do y=1,board_size_cl
          do dx=-1,1
             do dy=-1,1
                if (dx /= 0 .or. dy /= 0) then
                   line = tline([x, y], [dx, dy])
                   p = line%p + (strike_size_cl - 1)*line%d
                   if (in_bounds(p) .and. (.not. visited(x + dx, y + dy))) then
                      ok = check_line(board, player, line, strike_size_cl)
                      visited(p(1) - dx, p(2) - dy) = .true.
                      if (ok) return
                   end if
                end if
             end do
          end do
       end do
    end do
  end function player_won

  function check_line(board, player, line, steps) result(ok)
    integer,     intent(in)  :: board(board_size_cl, board_size_cl)
    integer,     intent(in)  :: player, steps
    type(TLine), intent(out) :: line
    logical :: ok
    integer :: i

    integer,dimension(2) :: p

    p = line%p + (steps-1)*line%d
    if (.not. in_bounds(p)) then
       ok = .false.
       return
    end if

    ok = .true.
    do i=1,steps
       p = line%p + (i-1)*line%d
       ok = board(p(1), p(2)) == player
       if (.not. ok) return
    end do
  end function check_line
end module game
