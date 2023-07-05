#include "macros.h"

module ui
  use iso_c_binding, only: c_null_char, c_int32_t
  use raylib
  implicit none

  integer(c_int32_t), parameter :: cell_regular_color     = color(z'FF252525')
  integer(c_int32_t), parameter :: cell_highlighted_color = color(z'FF353535')
  integer(c_int32_t), parameter :: knot_color             = color(z'FF3030FF')
  integer(c_int32_t), parameter :: cross_color            = color(z'FFFF6030')

contains
  function restart_button(button_font, rec, color) result(clicked)
    implicit none
    type(Font),intent(in) :: button_font
    type(Rectangle),intent(in) :: rec
    integer(c_int32_t),intent(in) :: color
    logical :: clicked

    type(Vector2) :: text_pos, text_size

    clicked = .false.
    if (check_collision_point_rect(get_mouse_position(), rec)) then
       call draw_rectangle_rounded(rec, 0.1, 10, color_brightness(color, -0.15))
       clicked = is_mouse_button_pressed(MOUSE_BUTTON_LEFT)
    else
       call draw_rectangle_rounded(rec, 0.1, 10, color)
    end if

    text_size = measure_text_ex(button_font, "Restart"//C_NULL_CHAR, 48.0, 0.0)
    text_pos = Vector2(rec%x + rec%width/2 - text_size%x/2, rec%y + rec%height/2 - text_size%y/2)
    call draw_text_ex(button_font, "Restart"//C_NULL_CHAR, text_pos, 48.0, 0.0, WHITE)
  end function restart_button

  subroutine empty_cell(x_px, y_px, s_px, color)
    implicit none
    real, intent(in) :: x_px, y_px, s_px
    integer(c_int32_t) :: color
    call draw_rectangle_rounded(Rectangle(x_px, y_px, s_px, s_px), 0.1, 10, color)
  end subroutine empty_cell

  function empty_cell_clickable(x_px,y_px,s_px) result(clicked)
    implicit none
    real :: x_px, y_px, s_px, mouse_x_px, mouse_y_px
    logical :: clicked

    mouse_x_px = get_mouse_x()
    mouse_y_px = get_mouse_y()

    if (x_px <= mouse_x_px .AND. mouse_x_px < x_px + s_px .AND. &
         y_px <= mouse_y_px .AND. mouse_y_px < y_px + s_px) then
       call empty_cell(x_px, y_px, s_px, cell_highlighted_color)
       clicked = is_mouse_button_pressed(MOUSE_BUTTON_LEFT)
    else
       call empty_cell(x_px, y_px, s_px, cell_regular_color)
       clicked = .FALSE.
    end if
  end function empty_cell_clickable

  subroutine knot_cell(x_px,y_px,s_px)
    use iso_c_binding, only: c_float
    implicit none
    real :: x_px, y_px, s_px
    type(Vector2) :: center
    real :: thick, pad

    call empty_cell(x_px, y_px, s_px, cell_regular_color);

    thick = s_px*0.2
    pad = s_px*0.2

    center%x = x_px + s_px/2
    center%y = y_px + s_px/2
    call draw_ring(center, (s_px - pad)/2 - thick, (s_px - pad)/2, 0.0, 360.0, 100, knot_color)
  end subroutine knot_cell

  subroutine cross_cell(x_px,y_px,s_px)
    use iso_c_binding, only: c_float
    implicit none
    real :: x_px, y_px, s_px
    type(Vector2) :: startPos
    type(Vector2) :: endPos
    real :: thick, pad

    call empty_cell(x_px, y_px, s_px, cell_regular_color)

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
end module ui
