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
  function board_empty(board) result(ok)
    integer, intent(in) :: board(board_size_cl,board_size_cl)
    logical :: ok

    integer :: x, y

    ok = .true.
    do x=1,board_size_cl
       do y=1,board_size_cl
          ok = board(x, y) == 0
          if (.not. ok) return
       end do
    end do
  end function board_empty
  
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

  function player_won(board, player, place, line) result(ok)
    integer,     intent(in)  :: board(board_size_cl, board_size_cl)
    integer,     intent(in)  :: player
    integer,     intent(in)  :: place(2)
    type(TLine), intent(out) :: line
    logical :: ok
    integer :: i, a, b

    integer, parameter :: dirs(4,2) = reshape([ &
       1, 0, 1, -1, &
       0, 1, 1, 1 &
    ], shape(dirs))

    ok = .false.
    do i=1,4
       a = check_line(board, player, tline(place, dirs(i,:)))
       b = check_line(board, player, tline(place, -dirs(i,:)))
       if (a + b - 1 >= strike_size_cl) then
          line = tline(place - dirs(i,:)*(b-1), dirs(i,:)*(a + b - 1 - 1))
          ok = .true.
          return
       end if
    end do
  end function player_won

  function check_line(board, player, line) result(steps)
    integer,     intent(in) :: board(board_size_cl, board_size_cl)
    integer,     intent(in) :: player
    type(TLine), intent(in) :: line
    integer :: steps

    integer :: p(2)

    steps = 0
    p = line%p + steps*line%d
    do while (in_bounds(p))
       if (board(p(1), p(2)) /= player) exit
       steps = steps + 1
       p = line%p + steps*line%d
    end do
  end function check_line
end module game
