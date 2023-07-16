module game
  implicit none

  integer, parameter :: board_size_cl = 3

  enum, bind(C)
     enumerator :: CELL_EMPTY = 0
     enumerator :: CELL_CROSS
     enumerator :: CELL_KNOTT
  end enum

  type :: TLine
     integer, dimension(2) :: p, d
  end type TLine

contains
  function board_full(board) result(ok)
    integer,dimension(board_size_cl,board_size_cl),intent(in) :: board
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

  function player_won(board, player, line) result(ok)
    integer,dimension(board_size_cl, board_size_cl),intent(in) :: board
    integer,     intent(in)  :: player
    type(TLine), intent(out) :: line
    logical :: ok

    integer :: i

    ok = .false.
    do i=1,board_size_cl
       line = tline([i, 1], [0, 1])
       ok = check_line(board, player, line)
       if (ok) return

       line = tline([1, i], [1, 0])
       ok = check_line(board, player, line)
       if (ok) return
    end do

    line = tline([1, 1], [1, 1])
    ok = check_line(board, player, line)
    if (ok) return

    line = tline([board_size_cl, 1], [-1, 1])
    ok = check_line(board, player, line)
  end function player_won

  function check_line(board, player, line) result(ok)
    integer,dimension(board_size_cl, board_size_cl),intent(in) :: board
    integer,intent(in) :: player
    type(TLine),intent(out) :: line
    logical :: ok

    integer,dimension(2) :: p

    p = line%p
    ok = .true.
    do while (1 <= p(1) .and. p(1) <= board_size_cl .and. 1 <= p(2) .and. p(2) <= board_size_cl)
       ok = board(p(1), p(2)) == player
       if (.not. ok) return
       p = p + line%d
    end do
  end function check_line
end module game
