#include "macros.h"

module ui
  use iso_c_binding, only: c_null_char, c_int32_t
  use raylib
  use game
  implicit none

  type :: Button_Style
     integer(c_int32_t) :: color
     real :: hover, hold
  end type Button_Style

  enum, bind(C)
     enumerator :: BUTTON_UNPRESSED = 0
     enumerator :: BUTTON_HOVER
     enumerator :: BUTTON_HOLD
  end enum

  integer(c_int32_t), parameter :: cell_color              = color(z'FF252525')
  integer(c_int32_t), parameter :: knot_color              = color(z'FF3030F0')
  integer(c_int32_t), parameter :: cross_color             = color(z'FF30F060')
  integer(c_int32_t), parameter :: restart_button_color    = color(z'FFEEEEEE')
  integer(c_int32_t), parameter :: strikethrough_color     = color(z'FFEEEEEE')
  real,               parameter :: board_padding_rl        = 0.03
  real,               parameter :: board_margin_rl         = 0.05
  real,               parameter :: restart_button_width_rl = 0.3
  type(Button_Style), parameter :: cell_button_style = Button_Style( &
       color = cell_color, &
       hover = 0.10, &
       hold = 0.15)
  type(Button_Style), parameter :: restart_button_style = Button_Style( &
       color = restart_button_color, &
       hover = -0.10, &
       hold = -0.15)
  ! TODO: checkbox line thickness should be relative
  real, parameter :: checkbox_line_thickness_px = 10.0
  integer(c_int32_t), parameter :: checkbox_color = restart_button_color
  integer, parameter :: restart_button_id = board_size_cl*board_size_cl + 1
  integer, parameter :: cross_checkbox_id = restart_button_id + 1
  integer, parameter :: knott_checkbox_id = cross_checkbox_id + 1

  integer :: active_button_id = 0

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

    clicked = button(restart_button_id, rec, restart_button_style)

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

  function button_logic(id, boundary, state) result(clicked)
    integer,intent(in) :: id
    type(Rectangle),intent(in) :: boundary
    integer,intent(out) :: state
    logical :: clicked
    clicked = .FALSE.
    state = BUTTON_UNPRESSED
    if (active_button_id == 0) then
       if (check_collision_point_rect(get_mouse_position(), boundary)) then
          if (is_mouse_button_down(MOUSE_BUTTON_LEFT)) then
             state = BUTTON_HOLD
             active_button_id = id
          else
             state = BUTTON_HOVER
          end if
       else
          state = BUTTON_UNPRESSED
       end if
    else if (active_button_id == id) then
       if (is_mouse_button_released(MOUSE_BUTTON_LEFT)) then
          clicked = check_collision_point_rect(get_mouse_position(), boundary)
          active_button_id = 0
          state = BUTTON_UNPRESSED
       else
          state = BUTTON_HOLD
       end if
    else
       ! TODO: handle the situation when the active button was not rendered on mouse releasea
       state = BUTTON_UNPRESSED
    end if
  end function button_logic

  subroutine checkbox(id,boundary,state)
    implicit none
    integer,intent(in) :: id
    type(Rectangle),intent(in) :: boundary
    logical,intent(inout) :: state

    integer :: button_state

    if (button_logic(id, boundary, button_state)) then
       state = .not. state
    end if

    if (state) then
       select case (button_state)
       case (BUTTON_UNPRESSED)
          call draw_rectangle_rec(boundary, restart_button_color)
       case (BUTTON_HOVER)
          call draw_rectangle_rec( &
               boundary, &
               color_brightness(restart_button_color, restart_button_style%hover))
       case (BUTTON_HOLD)
          call draw_rectangle_rec( &
               boundary, &
               color_brightness(restart_button_color, restart_button_style%hold))
       end select
    else
       select case (button_state)
       case (BUTTON_UNPRESSED)
          call draw_rectangle_lines_ex( &
               boundary, &
               checkbox_line_thickness_px, &
               restart_button_color)
       case (BUTTON_HOVER)
          call draw_rectangle_lines_ex( &
               boundary, &
               checkbox_line_thickness_px, &
               color_brightness(restart_button_color, restart_button_style%hover))
       case (BUTTON_HOLD)
          call draw_rectangle_lines_ex( &
               boundary, &
               checkbox_line_thickness_px, &
               color_brightness(restart_button_color, restart_button_style%hold))
       end select
    end if
  end subroutine checkbox

  function button(id,boundary,style) result(clicked)
    implicit none
    integer,            intent(in) :: id
    type(Rectangle),    intent(in) :: boundary
    type(Button_Style), intent(in) :: style
    logical :: clicked

    integer :: state

    clicked = button_logic(id, boundary, state)
    select case (state)
    case (BUTTON_UNPRESSED)
       call draw_rectangle_rounded(boundary, 0.10, 10, style%color)
    case (BUTTON_HOVER)
       call draw_rectangle_rounded(boundary, 0.10, 10, color_brightness(style%color, style%hover))
    case (BUTTON_HOLD)
       call draw_rectangle_rounded(boundary, 0.10, 10, color_brightness(style%color, style%hold))
    end select
  end function button

  function empty_cell_clickable(id,x_px,y_px,s_px) result(clicked)
    implicit none
    integer,intent(in) :: id
    real,intent(in) :: x_px, y_px, s_px
    logical :: clicked
    clicked = button(id, rectangle(x_px, y_px, s_px, s_px), cell_button_style)
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

    integer :: x_cl, y_cl, id
    real :: cell_size_px, x_px, y_px, s_px

    cell_size_px = board_size_px/board_size_cl

    clicked = .false.
    do x_cl=1,board_size_cl
       do y_cl=1,board_size_cl
          id = (x_cl-1)*board_size_cl + (y_cl-1) + 1
          x_px = board_x_px + (x_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          y_px = board_y_px + (y_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
          s_px = cell_size_px - (cell_size_px*board_padding_rl)
          select case (board(x_cl, y_cl))
          case (CELL_EMPTY)
             if (.not. clicked) then
                clicked = empty_cell_clickable(id, x_px, y_px, s_px)
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
