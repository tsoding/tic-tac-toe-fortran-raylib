#include "macros.h"

module ui
  use iso_c_binding, only: c_null_char, c_int32_t
  use raylib
  use game
  implicit none

  integer(c_int32_t), parameter :: cell_color              = color(z'FF252525')
  integer(c_int32_t), parameter :: knot_color              = color(z'FF3030F0')
  integer(c_int32_t), parameter :: cross_color             = color(z'FF30F060')
  integer(c_int32_t), parameter :: restart_button_color    = color(z'FFEEEEEE')
  integer(c_int32_t), parameter :: strikethrough_color     = color(z'FFEEEEEE')
  real,               parameter :: board_padding_rl        = 0.03
  real,               parameter :: restart_button_width_rl = 0.3

contains
  function restart_button(button_font, board_x_px, board_y_px, board_size_px) result(clicked)
    implicit none
    type(Font),intent(in) :: button_font
    real,intent(in) :: board_x_px, board_y_px, board_size_px
    logical :: clicked

    type(Vector2) :: text_pos, text_size
    type(Rectangle) :: rec

    rec%width = board_size_px*restart_button_width_rl
    rec%height = rec%width*0.4
    rec%x = board_x_px + board_size_px/2 - rec%width/2
    rec%y = board_y_px + board_size_px/2 - rec%height/2

    clicked = .false.
    if (check_collision_point_rect(get_mouse_position(), rec)) then
       if (is_mouse_button_down(MOUSE_BUTTON_LEFT)) then
          call draw_rectangle_rounded(rec, 0.1, 10, color_brightness(restart_button_color, -0.30))
       else
          call draw_rectangle_rounded(rec, 0.1, 10, color_brightness(restart_button_color, -0.15))
       end if
       clicked = is_mouse_button_released(MOUSE_BUTTON_LEFT)
    else
       call draw_rectangle_rounded(rec, 0.1, 10, restart_button_color)
    end if

    text_size = measure_text_ex(button_font, "Restart"//C_NULL_CHAR, rec%height*0.4, 0.0)
    text_pos = Vector2(rec%x + rec%width/2 - text_size%x/2, rec%y + rec%height/2 - text_size%y/2)
    call draw_text_ex(button_font, "Restart"//C_NULL_CHAR, text_pos, rec%height*0.4, 0.0, BLACK)
  end function restart_button

  subroutine empty_cell(x_px, y_px, s_px, color)
    implicit none
    real, intent(in) :: x_px, y_px, s_px
    integer(c_int32_t) :: color
    call draw_rectangle_rounded(Rectangle(x_px, y_px, s_px, s_px), 0.1, 10, color)
  end subroutine empty_cell

  ! TODO: we want the clickable cells behave like the restart button
  ! Steps to reproduce the problem
  ! 1. start the game
  ! 2. click cells right in the center in the following order (1,2) (1,3) (2,3) (2,2) [format is (x, y)]
  ! 3. observe that click on (2,2) also clicked the restart button
  ! If clickable cells were activated on release, this would not happen
  function empty_cell_clickable(x_px,y_px,s_px) result(clicked)
    implicit none
    real,intent(in) :: x_px, y_px, s_px
    logical :: clicked

    if (check_collision_point_rect(get_mouse_position(), rectangle(x_px, y_px, s_px, s_px))) then
       call empty_cell(x_px, y_px, s_px, color_brightness(cell_color, 0.10))
       clicked = is_mouse_button_pressed(MOUSE_BUTTON_LEFT)
    else
       call empty_cell(x_px, y_px, s_px, cell_color)
       clicked = .FALSE.
    end if
  end function empty_cell_clickable

  subroutine knot_cell(x_px,y_px,s_px)
    implicit none
    real :: x_px, y_px, s_px
    type(Vector2) :: center
    real :: thick, pad

    call empty_cell(x_px, y_px, s_px, cell_color);

    thick = s_px*0.2
    pad = s_px*0.2

    center%x = x_px + s_px/2
    center%y = y_px + s_px/2
    call draw_ring(center, (s_px - pad)/2 - thick, (s_px - pad)/2, 0.0, 360.0, 100, knot_color)
  end subroutine knot_cell

  subroutine cross_cell(x_px,y_px,s_px)
    implicit none
    real :: x_px, y_px, s_px
    type(Vector2) :: startPos
    type(Vector2) :: endPos
    real :: thick, pad

    call empty_cell(x_px, y_px, s_px, cell_color)

    thick = s_px*0.2
    pad = s_px*0.2

    startPos%x = x_px + pad
    startPos%y = y_px + pad
    endPos%x   = x_px + s_px - pad
    endPos%y   = y_px + s_px - pad
    call draw_line_ex(startPos, endPos, thick, cross_color)

    startPos%x = x_px + s_px - pad
    startPos%y = y_px + pad
    endPos%x   = x_px + pad
    endPos%y   = y_px + s_px - pad
    call draw_line_ex(startPos, endPos, thick, cross_color)
  end subroutine cross_cell

  subroutine render_board(board_x_px, board_y_px, board_size_px, board)
    implicit none

    real,intent(in) :: board_x_px, board_y_px, board_size_px
    integer,dimension(board_size_cl,board_size_cl),intent(in) :: board

    integer :: x_cl, y_cl
    real :: cell_size_px, x_px, y_px, s_px

    cell_size_px = board_size_px/board_size_cl

    do x_cl=1,board_size_cl
       do y_cl=1,board_size_cl
          x_px = board_x_px + (x_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          y_px = board_y_px + (y_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          s_px = cell_size_px - (cell_size_px*board_padding_rl)
          select case (board(x_cl, y_cl))
          case (CELL_EMPTY)
             call empty_cell(x_px, y_px, s_px, cell_color)
          case (CELL_CROSS)
             call cross_cell(x_px, y_px, s_px)
          case (CELL_KNOTT)
             call knot_cell(x_px, y_px, s_px)
          end select
       end do
    end do
  end subroutine render_board

  function render_board_clickable(board_x_px, board_y_px, board_size_px, board, clicked_x_cl, clicked_y_cl) result(clicked)
    implicit none

    real,intent(in) :: board_x_px, board_y_px, board_size_px
    integer,dimension(board_size_cl,board_size_cl),intent(in) :: board
    integer,intent(out) :: clicked_x_cl, clicked_y_cl
    logical :: clicked

    integer :: x_cl, y_cl
    real :: cell_size_px, x_px, y_px, s_px

    cell_size_px = board_size_px/board_size_cl

    clicked = .false.
    do x_cl=1,board_size_cl
       do y_cl=1,board_size_cl
          x_px = board_x_px + (x_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          y_px = board_y_px + (y_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          s_px = cell_size_px - (cell_size_px*board_padding_rl)
          select case (board(x_cl, y_cl))
          case (CELL_EMPTY)
             if (.not. clicked) then
                clicked = empty_cell_clickable(x_px, y_px, s_px)
                if (clicked) then
                   clicked_x_cl = x_cl
                   clicked_y_cl = y_cl
                end if
             else
                ! If we already clicked a cell the rest of the cells stay disabled until the end of the frame
                call empty_cell(x_px, y_px, s_px, cell_color)
             end if
          case (CELL_CROSS)
             call cross_cell(x_px, y_px, s_px)
          case (CELL_KNOTT)
             call knot_cell(x_px, y_px, s_px)
          end select
       end do
    end do
  end function render_board_clickable

  subroutine strikethrough(final_line, board_x_px, board_y_px, board_size_px)
    implicit none
    type(TLine),intent(in) :: final_line
    real,intent(in) :: board_x_px, board_y_px, board_size_px

    real :: thick, cell_size_px
    type(Vector2) :: startPos, endPos

    cell_size_px = board_size_px/board_size_cl

    thick = cell_size_px*0.2
    startPos%x = board_x_px + (final_line%x-1)*cell_size_px + cell_size_px/2 + (-final_line%dx)*(cell_size_px/3)
    startPos%y = board_y_px + (final_line%y-1)*cell_size_px + cell_size_px/2 + (-final_line%dy)*(cell_size_px/3)
    endPos%x   = board_x_px + ((final_line%x-1) + 2*final_line%dx)*cell_size_px + cell_size_px/2 + final_line%dx*(cell_size_px/3)
    endPos%y   = board_y_px + ((final_line%y-1) + 2*final_line%dy)*cell_size_px + cell_size_px/2 + final_line%dy*(cell_size_px/3)
    call draw_line_ex(startPos, endPos, thick, strikethrough_color)
  end subroutine strikethrough
end module ui
