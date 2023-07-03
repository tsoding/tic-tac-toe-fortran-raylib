#include "macros.h"

program main
  use iso_c_binding, only: c_int, c_int32_t, C_NULL_CHAR
  use raylib
  use game
  implicit none

  ! Measure units:
  ! - *_px - pixels
  ! - *_cl - cells
  ! - *_rl - relative (fraction of width, height, cell size, etc)
  integer(c_int),     parameter :: screen_width_px        = 800
  integer(c_int),     parameter :: screen_height_px       = 600
  integer(c_int),     parameter :: fps                    = 60
  integer(c_int32_t), parameter :: cell_regular_color     = color(z'FF252525')
  integer(c_int32_t), parameter :: cell_highlighted_color = color(z'FF353535')
  integer(c_int32_t), parameter :: cross_color            = color(z'FFFF3030')
  integer(c_int32_t), parameter :: knot_color             = color(z'FF3030FF')
  integer(c_int32_t), parameter :: background_color       = color(z'FF181818')
  integer(c_int32_t), parameter :: strikethrough_color    = color(z'FFFFFFFF')
  real,               parameter :: board_padding_rl       = 0.03

  real    :: dt
  integer :: x_cl, y_cl, next_x_cl, next_y_cl
  real    :: x_px, y_px, w_px, h_px
  integer :: window_width_px, window_height_px
  real    :: board_x_px, board_y_px, board_size_px, cell_size_px

  integer,dimension(board_size_cl, board_size_cl) :: board

  integer :: current_player
  type(TLine) :: final_line
  integer :: state

  integer, parameter :: STATE_GAME = 0
  integer, parameter :: STATE_WON  = 1
  integer, parameter :: STATE_TIE  = 2

  state = STATE_GAME
  board(:,:) = reshape((/ &
      0, 0, 0, &
      0, 0, 0, &
      0, 0, 0  &
  /), (/board_size_cl, board_size_cl/))

  current_player = CELL_CROSS

  call set_config_flags(FLAG_WINDOW_RESIZABLE)
  call init_window(screen_width_px, screen_height_px, "Fortran GOTY"//C_NULL_CHAR)
  call set_target_fps(fps)
  do while (.not. window_should_close())
     dt               = get_frame_time()
     window_width_px  = get_render_width()
     window_height_px = get_render_height()

     if (window_width_px > window_height_px) then
        board_size_px = window_height_px
        board_x_px = real(window_width_px)/2 - board_size_px/2
        board_y_px = 0
     else
        board_size_px = window_width_px
        board_x_px = 0
        board_y_px = real(window_height_px)/2 - board_size_px/2
     end if

     cell_size_px = board_size_px/board_size_cl

     call begin_drawing()
     call clear_background(background_color)
     select case (state)
     case (STATE_GAME)
        call render_game_state()
     case (STATE_WON)
        call render_won_state()
     case (STATE_TIE)
        call render_tie_state()
     end select
     call end_drawing()
  end do

contains
  recursive function who_wins(player, x, y) result(who)
    implicit none
    integer, intent(in) :: player, x, y
    type(TLine) :: ignore
    integer :: who, opponent, next
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
             next = who_wins(opponent, ix, iy)
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
  end function who_wins

  function ai_next_move(player, ox, oy) result(giveup)
    implicit none
    integer, intent(in)  :: player
    integer, intent(out) :: ox, oy
    logical :: giveup
    integer :: x, y, next
    giveup = .true.
    do x=1,board_size_cl
       do y=1,board_size_cl
          if (board(x, y) == CELL_EMPTY) then
             next = who_wins(player, x, y)
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

  subroutine render_tie_state()
    implicit none
    do x_cl=1,board_size_cl
       do y_cl=1,board_size_cl
          x_px = board_x_px + (x_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          y_px = board_y_px + (y_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          w_px = cell_size_px - (cell_size_px*board_padding_rl)
          h_px = cell_size_px - (cell_size_px*board_padding_rl)
          select case (board(x_cl, y_cl))
          case (CELL_EMPTY)
             call empty_cell_disabled(x_px, y_px, w_px, h_px)
          case (CELL_CROSS)
             call cross_cell(x_px, y_px, w_px, h_px)
          case (CELL_KNOTT)
             call knot_cell(x_px, y_px, w_px, h_px)
          end select
       end do
    end do
  end subroutine render_tie_state

  subroutine render_won_state()
    implicit none
    type(Vector2) :: startPos, endPos
    real :: thick
    do x_cl=1,board_size_cl
       do y_cl=1,board_size_cl
          x_px = board_x_px + (x_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          y_px = board_y_px + (y_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          w_px = cell_size_px - (cell_size_px*board_padding_rl)
          h_px = cell_size_px - (cell_size_px*board_padding_rl)
          select case (board(x_cl, y_cl))
          case (CELL_EMPTY)
             call empty_cell_disabled(x_px, y_px, w_px, h_px)
          case (CELL_CROSS)
             call cross_cell(x_px, y_px, w_px, h_px)
          case (CELL_KNOTT)
             call knot_cell(x_px, y_px, w_px, h_px)
          end select
       end do
    end do

    thick = cell_size_px*0.2

    startPos%x = board_x_px + (final_line%x-1)*cell_size_px + cell_size_px/2 + (-final_line%dx)*(cell_size_px/3)
    startPos%y = board_y_px + (final_line%y-1)*cell_size_px + cell_size_px/2 + (-final_line%dy)*(cell_size_px/3)
    endPos%x   = board_x_px + ((final_line%x-1) + 2*final_line%dx)*cell_size_px + cell_size_px/2 + final_line%dx*(cell_size_px/3)
    endPos%y   = board_y_px + ((final_line%y-1) + 2*final_line%dy)*cell_size_px + cell_size_px/2 + final_line%dy*(cell_size_px/3)
    call draw_line_ex(startPos, endPos, thick, strikethrough_color)
  end subroutine render_won_state

  subroutine render_game_state()
    implicit none
    select case(current_player)
    case (CELL_CROSS)
       do x_cl=1,board_size_cl
          do y_cl=1,board_size_cl
             x_px = board_x_px + (x_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
             y_px = board_y_px + (y_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
             w_px = cell_size_px - (cell_size_px*board_padding_rl)
             h_px = cell_size_px - (cell_size_px*board_padding_rl)
             select case (board(x_cl, y_cl))
             case (CELL_EMPTY)
                if (empty_cell_clickable(x_px, y_px, w_px, h_px)) then
                   board(x_cl, y_cl) = current_player
                   if (player_won(board, CELL_CROSS, final_line)) then
                      state = STATE_WON
                      return
                   end if
                   if (player_won(board, CELL_KNOTT, final_line)) then
                      state = STATE_WON
                      return
                   end if
                   current_player = 3 - current_player
                end if
             case (CELL_CROSS)
                call cross_cell(x_px, y_px, w_px, h_px)
             case (CELL_KNOTT)
                call knot_cell(x_px, y_px, w_px, h_px)
             end select
          end do
       end do
    case (CELL_KNOTT)
       do x_cl=1,board_size_cl
          do y_cl=1,board_size_cl
             x_px = board_x_px + (x_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
             y_px = board_y_px + (y_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
             w_px = cell_size_px - (cell_size_px*board_padding_rl)
             h_px = cell_size_px - (cell_size_px*board_padding_rl)
             select case (board(x_cl, y_cl))
             case (CELL_EMPTY)
                call empty_cell_disabled(x_px, y_px, w_px, h_px)
             case (CELL_CROSS)
                call cross_cell(x_px, y_px, w_px, h_px)
             case (CELL_KNOTT)
                call knot_cell(x_px, y_px, w_px, h_px)
             end select
          end do
       end do
       if (.not. ai_next_move(current_player, next_x_cl, next_y_cl)) then
          board(next_x_cl, next_y_cl) = current_player
          if (player_won(board, CELL_CROSS, final_line)) then
             state = STATE_WON
             return
          end if
          if (player_won(board, CELL_KNOTT, final_line)) then
             state = STATE_WON
             return
          end if
          current_player = 3 - current_player
       else
          state = STATE_TIE
       end if
    end select
  end subroutine render_game_state

  subroutine empty_cell_disabled(x_px,y_px,w_px,h_px)
    implicit none
    real :: x_px, y_px, w_px, h_px
    call draw_rectangle(int(x_px), int(y_px), int(w_px), int(h_px), cell_regular_color)
  end subroutine empty_cell_disabled

  function empty_cell_clickable(x_px,y_px,w_px,h_px) result(clicked)
    implicit none
    real :: x_px, y_px, w_px, h_px, mouse_x_px, mouse_y_px
    logical :: clicked

    mouse_x_px = get_mouse_x()
    mouse_y_px = get_mouse_y()

    if (x_px <= mouse_x_px .AND. mouse_x_px < x_px + w_px .AND. &
         y_px <= mouse_y_px .AND. mouse_y_px < y_px + h_px) then
       call draw_rectangle(int(x_px), int(y_px), int(w_px), int(h_px), cell_highlighted_color)
       clicked = is_mouse_button_pressed(MOUSE_BUTTON_LEFT)
    else
       call draw_rectangle(int(x_px), int(y_px), int(w_px), int(h_px), cell_regular_color)
       clicked = .FALSE.
    end if
  end function empty_cell_clickable

  subroutine cross_cell(x_px,y_px,w_px,h_px)
    use iso_c_binding, only: c_float
    implicit none
    real :: x_px, y_px, w_px, h_px
    type(Vector2) :: startPos
    type(Vector2) :: endPos
    real :: thick, pad

    call draw_rectangle(int(x_px), int(y_px), int(w_px), int(h_px), cell_regular_color)

    thick = w_px*0.2
    pad = w_px*0.2

    startPos%x = x_px + pad
    startPos%y = y_px + pad
    endPos%x   = x_px + w_px - pad
    endPos%y   = y_px + h_px - pad
    call draw_line_ex(startPos, endPos, thick, cross_color)

    startPos%x = x_px + w_px - pad
    startPos%y = y_px + pad
    endPos%x   = x_px + pad
    endPos%y   = y_px + h_px - pad
    call draw_line_ex(startPos, endPos, thick, cross_color)
  end subroutine cross_cell

  subroutine knot_cell(x_px,y_px,w_px,h_px)
    use iso_c_binding, only: c_float
    implicit none
    real :: x_px, y_px, w_px, h_px
    type(Vector2) :: center
    real :: thick, pad

    call draw_rectangle(int(x_px), int(y_px), int(w_px), int(h_px), cell_regular_color)

    thick = w_px*0.2
    pad = w_px*0.2

    center%x = x_px + w_px/2
    center%y = y_px + h_px/2
    call draw_ring(center, (w_px - pad)/2 - thick, (w_px - pad)/2, 0.0, 360.0, 100, knot_color)
  end subroutine knot_cell
end program
