module ai
  use game
  implicit none
contains
  recursive function ai_who_wins(board, player, x, y) result(who)
    implicit none
    integer, dimension(board_size_cl,board_size_cl), intent(inout) :: board
    integer, intent(in) :: player, x, y
    integer :: who

    type(TLine) :: ignore
    integer :: opponent, next
    integer :: ix, iy

    board(x, y) = player

    if (player_won(board, CELL_CROSS, ignore)) then
       who = CELL_CROSS
       board(x, y) = 0
       return
    end if

    if (player_won(board, CELL_KNOTT, ignore)) then
       who = CELL_KNOTT
       board(x, y) = 0
       return
    end if

    if (board_full(board)) then
       who = 0
       board(x, y) = 0
       return
    end if

    opponent = 3 - player

    who = player
    do ix=1,board_size_cl
       do iy=1,board_size_cl
          if (board(ix, iy) == CELL_EMPTY) then
             next = ai_who_wins(board, opponent, ix, iy)
             if (next == 0) who = 0
             if (next == opponent) then
                who = opponent
                board(x, y) = 0
                return
             end if
          end if
       end do
    end do
    board(x, y) = 0
  end function ai_who_wins

  function ai_next_move(board, player, ox, oy) result(giveup)
    implicit none
    integer, dimension(board_size_cl,board_size_cl), intent(inout) :: board
    integer, intent(in)  :: player
    integer, intent(out) :: ox, oy
    logical :: giveup
    integer :: x, y, next
    giveup = .true.
    do x=1,board_size_cl
       do y=1,board_size_cl
          if (board(x, y) == CELL_EMPTY) then
             next = ai_who_wins(board, player, x, y)
             if (next == 0) then
                giveup = .false.
                ox = x
                oy = y
             end if
             if (next == player) then
                giveup = .false.
                ox = x
                oy = y
                return
             end if
          end if
       end do
    end do
  end function ai_next_move

end module ai
