! TODO: AI is too slow for border sizes > 3
! Speed up ideas to try:
! - memoization
! - limit the depth of recursion
! - alpha-beta pruning
module ai
  use game
  implicit none

  integer, parameter :: ai_max_depth = 6
  integer, parameter :: ai_max_breadth = 3

contains
  recursive function ai_who_wins(board, player, x, y, fturns, depth) result(who)
    integer, dimension(board_size_cl,board_size_cl), intent(inout) :: board
    integer, intent(in) :: player, x, y, depth
    integer, intent(out) :: fturns
    integer :: who

    type(TLine) :: ignore
    integer :: cturns
    integer :: opponent, next
    integer :: ix, iy, i, j
    integer :: xs(2)
    integer :: empty_cells(board_size_cl*board_size_cl, 2), empty_cells_count
    real :: t
    empty_cells_count = 0

    if (depth >= ai_max_depth) then
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

    do ix=1,board_size_cl
       do iy=1,board_size_cl
          if (board(ix, iy) == CELL_EMPTY .and. has_neighbors(board, ix, iy)) then
             empty_cells_count = empty_cells_count + 1
             empty_cells(empty_cells_count, :) = [ix, iy]
          end if
       end do
    end do

    do i=1,empty_cells_count
       call random_number(t)
       j = int(i + (empty_cells_count - i)*t)
       xs = empty_cells(i, :)
       empty_cells(i, :) = empty_cells(j, :)
       empty_cells(j, :) = xs
    end do

    who = player
    do i=1,min(empty_cells_count, ai_max_breadth)
       next = ai_who_wins(board, opponent, empty_cells(i, 1), empty_cells(i, 2), cturns, depth + 1)
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
    end do
    board(x, y) = 0
  end function ai_who_wins

  function ai_next_move(board, player, ox, oy) result(giveup)
    integer, dimension(board_size_cl,board_size_cl), intent(inout) :: board
    integer, intent(in)  :: player
    integer, intent(out) :: ox, oy
    logical :: giveup
    integer :: next, winner, i, j, ix, iy, x, y, xs(2)
    integer :: fturns, cturns
    integer :: empty_cells(board_size_cl*board_size_cl, 2), empty_cells_count
    real :: t
    empty_cells_count = 0

    fturns = huge(fturns)
    winner = 3-player
    giveup = .true.

    if (board_empty(board)) then
       giveup = .false.
       call random_number(t)
       ox = 1 + (board_size_cl - 1)*t
       oy = 1 + (board_size_cl - 1)*t
       return
    end if

    do ix=1,board_size_cl
       do iy=1,board_size_cl
          if (board(ix, iy) == CELL_EMPTY .and. has_neighbors(board, ix, iy)) then
             empty_cells_count = empty_cells_count + 1
             empty_cells(empty_cells_count, :) = [ix, iy]
          end if
       end do
    end do

    do i=1,empty_cells_count
       call random_number(t)
       j = int(i + (empty_cells_count - i)*t)
       xs = empty_cells(i, :)
       empty_cells(i, :) = empty_cells(j, :)
       empty_cells(j, :) = xs
    end do


    do i=1,empty_cells_count
       x = empty_cells(i, 1)
       y = empty_cells(i, 2)
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
    end do
  end function ai_next_move

  function has_neighbors(board, x, y) result(yes)
    integer, intent(in) :: board(board_size_cl, board_size_cl), x, y
    logical :: yes
    integer :: dx, dy
    yes = .false.
    do dx=-1,1
       do dy=-1,1
          if (dx /= 0 .or. dy /= 0) then
             if (in_bounds([x + dx, y + dy])) then
                yes = board(x + dx, y + dy) /= CELL_EMPTY
                if (yes) return
             end if
          end if
       end do
    end do
  end function has_neighbors

end module ai
