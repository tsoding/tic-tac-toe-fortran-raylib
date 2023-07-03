#include "macros.h"

module raylib
  use iso_c_binding, only: c_int32_t, c_char, c_int, c_bool, c_float
  implicit none
  type, bind(C) :: Vector2
     real(c_float) :: x, y
  end type Vector2

  ! TODO: use the Raylib colors
  integer(c_int32_t), parameter :: BLANK = 0
  integer(c_int32_t), parameter :: BLACK = color(z'FF000000')
  integer(c_int32_t), parameter :: RED   = color(z'FF0000FF')
  integer(c_int32_t), parameter :: GREEN = color(z'FF00FF00')
  integer(c_int32_t), parameter :: BLUE  = color(z'FFFF0000')
  integer(c_int32_t), parameter :: MOUSE_BUTTON_LEFT = 0

  integer(c_int32_t), parameter :: FLAG_WINDOW_RESIZABLE = hex32(z'00000004')

  interface
     subroutine init_window(width,height,title) bind(C, name="InitWindow")
       use iso_c_binding, only: c_char, c_int
       integer(c_int),value :: width
       integer(c_int),value :: height
       character(kind=c_char) :: title(*)
     end subroutine init_window

     subroutine set_target_fps(fps) bind(C, name="SetTargetFPS")
       use iso_c_binding, only: c_int
       integer(c_int),value :: fps
     end subroutine set_target_fps

     logical(c_bool) function window_should_close() bind(C, name="WindowShouldClose")
       use iso_c_binding, only: c_bool
     end function window_should_close

     real(c_float) function get_frame_time() bind(C, name="GetFrameTime")
       use iso_c_binding, only: c_float
     end function get_frame_time

     subroutine begin_drawing() bind(C, name="BeginDrawing")
     end subroutine begin_drawing

     subroutine end_drawing() bind(C, name="EndDrawing")
     end subroutine end_drawing

     subroutine clear_background(color) bind(C, name="ClearBackground")
       use iso_c_binding, only: c_int32_t
       integer(c_int32_t),value :: color
     end subroutine clear_background

     subroutine draw_rectangle(x,y,w,h,color) bind(C, name="DrawRectangle")
       use iso_c_binding, only: c_int, c_int32_t
       integer(c_int),value :: x
       integer(c_int),value :: y
       integer(c_int),value :: w
       integer(c_int),value :: h
       integer(c_int32_t),value :: color
     end subroutine draw_rectangle

     subroutine set_config_flags(flags) bind(C, name="SetConfigFlags")
       use iso_c_binding, only: c_int32_t
       integer(c_int32_t),value :: flags
     end subroutine set_config_flags

     integer(c_int) function get_render_width() bind(C, name="GetRenderWidth")
       use iso_c_binding, only: c_int
     end function get_render_width

     integer(c_int) function get_render_height() bind(C, name="GetRenderHeight")
       use iso_c_binding, only: c_int
     end function get_render_height

     integer(c_int) function get_mouse_x() bind(C, name="GetMouseX")
       use iso_c_binding, only: c_int
     end function get_mouse_x

     integer(c_int) function get_mouse_y() bind(C, name="GetMouseY")
       use iso_c_binding, only: c_int
     end function get_mouse_y

     subroutine draw_line_ex(startPos,endPos,thick,color) bind(C, name="DrawLineEx")
       use iso_c_binding, only: c_int, c_float, c_int32_t
       type, bind(C) :: Vector2
          real(c_float) :: x, y
       end type Vector2
       type(Vector2),value      :: startPos
       type(Vector2),value      :: endPos
       real(c_float),value      :: thick
       integer(c_int32_t),value :: color
     end subroutine draw_line_ex

     ! DrawRing(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color)
     subroutine draw_ring(center,innerRadius,outerRadius,startAngle,endAngle,segments,color) bind(C, name="DrawRing")
       use iso_c_binding, only: c_float, c_int, c_int32_t
       type, bind(C) :: Vector2
          real(c_float) :: x, y
       end type Vector2
       type(Vector2),value      :: center
       real(c_float),value      :: innerRadius
       real(c_float),value      :: outerRadius
       real(c_float),value      :: startAngle
       real(c_float),value      :: endAngle
       integer(c_int),value     :: segments
       integer(c_int32_t),value :: color
     end subroutine draw_ring

     logical(c_bool) function is_mouse_button_pressed(button) bind(C, name="IsMouseButtonPressed")
       use iso_c_binding, only: c_int, c_bool
       integer(c_int),value :: button
     end function is_mouse_button_pressed
  end interface
end module raylib
