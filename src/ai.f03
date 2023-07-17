! TODO: AI is too slow for border sizes > 3
! Speed up ideas to try:
! - memoization
! - limit the depth of recursion
! - alpha-beta pruning
module ai
  use game
  implicit none
contains
  recursive function ai_who_wins(board, player, x, y, fturns, depth) result(who)
    integer, dimension(board_size_cl,board_size_cl), intent(inout) :: board
    integer, intent(in) :: player, x, y, depth
    integer, intent(out) :: fturns
    integer :: who

    type(TLine) :: ignore
    integer :: cturns
    integer :: opponent, next
    integer :: ix, iy

    if (depth >= 7) then
       who = 0
       return
    end if

    fturns = 1
    board(x, y) = player

    if (player_won(board, CELL_CROSS, [x, y], ignore)) then
       who = CELL_CROSS
       board(x, y) = 0
       return
    end if

    if (player_won(board, CELL_KNOTT, [x, y], ignore)) then
       who = CELL_KNOTT
       board(x, y) = 0
       return
    end if

    if (board_full(board)) then
       who = 0
       board(x, y) = 0
       return
    end if

    fturns = huge(fturns)
    opponent = 3 - player

    who = player
    do ix=1,board_size_cl
       do iy=1,board_size_cl
          if (board(ix, iy) == CELL_EMPTY) then
             next = ai_who_wins(board, opponent, ix, iy, cturns, depth + 1)
             cturns = cturns + 1  
             if (next == player .and. who == player .and. cturns < fturns) then
                fturns = cturns
             end if
             if (next == 0) then
                fturns = cturns
                who = 0
             end if
             if (next == opponent) then
                fturns = cturns
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
    integer, dimension(board_size_cl,board_size_cl), intent(inout) :: board
    integer, intent(in)  :: player
    integer, intent(out) :: ox, oy
    logical :: giveup
    integer :: x, y, next, winner
    integer :: fturns, cturns
    
    fturns = huge(fturns)
    winner = 3-player
    giveup = .true.

    do x=1,board_size_cl
       do y=1,board_size_cl
          if (board(x, y) == CELL_EMPTY) then
             ! Play in the first empty space found if we're going to loose anyway 
             if(giveup) then
               giveup = .false.
               ox = x
               oy = y
             end if

             next = ai_who_wins(board, player, x, y, cturns, 1)
             
             if (next == 0 .and. winner /= player) then
                giveup = .false.
                winner = 0
                ox = x
                oy = y
             end if
            
             if (next == player .and. cturns < fturns) then
                giveup = .false.
                winner = player
                ox = x
                oy = y
                fturns = cturns

                if(fturns == 1) then
                   return
                end if
             end if
          end if
       end do
    end do
  end function ai_next_move

end module ai
