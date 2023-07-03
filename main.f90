#include "macros.h"

program main
  use iso_c_binding, only: c_int, c_int32_t, C_NULL_CHAR
  use raylib
  implicit none

  ! Measure units:
  ! - *_px - pixels
  ! - *_cl - cells
  ! - *_rl - relative (fraction of width, height, cell size, etc)
  integer(c_int),     parameter :: screen_width_px        = 800
  integer(c_int),     parameter :: screen_height_px       = 600
  integer(c_int),     parameter :: fps                    = 60
  integer(c_int32_t), parameter :: cell_color_regular     = color(z'FF252525')
  integer(c_int32_t), parameter :: cell_color_highlighted = color(z'FF353535')
  integer(c_int32_t), parameter :: cross_color            = color(z'FFFF3030')
  integer(c_int32_t), parameter :: knot_color             = color(z'FF3030FF')
  integer(c_int32_t), parameter :: background_color       = color(z'FF181818')
  integer(c_int),     parameter :: board_size_cl          = 3
  real,               parameter :: board_padding_rl       = 0.03

  real    :: dt
  integer :: x_cl, y_cl
  real    :: x_px, y_px, w_px, h_px
  integer :: window_width_px, window_height_px
  real    :: board_x_px, board_y_px, board_size_px, cell_size_px

  integer,dimension(board_size_cl, board_size_cl) :: board
  integer,parameter                               :: CELL_EMPTY = 0
  integer,parameter                               :: CELL_CROSS = 1
  integer,parameter                               :: CELL_KNOTT = 2

  integer :: current_player

  board(:,:) = 0
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

     do x_cl=1,board_size_cl
        do y_cl=1,board_size_cl
           x_px = board_x_px + (x_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
           y_px = board_y_px + (y_cl - 1)*cell_size_px + (cell_size_px*board_padding_rl)/2
           w_px = cell_size_px - (cell_size_px*board_padding_rl)
           h_px = cell_size_px - (cell_size_px*board_padding_rl)
           select case (board(x_cl, y_cl))
              case (CELL_EMPTY)
                 if (empty_cell(x_px, y_px, w_px, h_px)) then
                    board(x_cl, y_cl) = current_player
                    current_player = 3 - current_player
                 end if
              case (CELL_CROSS)
                 call cross_cell(x_px, y_px, w_px, h_px)
              case (CELL_KNOTT)
                 call knot_cell(x_px, y_px, w_px, h_px)
           end select
        end do
     end do

     call end_drawing()
  end do

contains
  logical function empty_cell(x_px,y_px,w_px,h_px)
    implicit none
    real :: x_px, y_px, w_px, h_px, mouse_x_px, mouse_y_px

    mouse_x_px = get_mouse_x()
    mouse_y_px = get_mouse_y()

    if (x_px <= mouse_x_px .AND. mouse_x_px < x_px + w_px .AND. &
         y_px <= mouse_y_px .AND. mouse_y_px < y_px + h_px) then
       call draw_rectangle(int(x_px), int(y_px), int(w_px), int(h_px), cell_color_highlighted)
       empty_cell = is_mouse_button_pressed(MOUSE_BUTTON_LEFT)
    else
       call draw_rectangle(int(x_px), int(y_px), int(w_px), int(h_px), cell_color_regular)
       empty_cell = .FALSE.
    end if
  end function empty_cell

  subroutine cross_cell(x_px,y_px,w_px,h_px)
    use iso_c_binding, only: c_float
    implicit none
    real :: x_px, y_px, w_px, h_px
    type(Vector2) :: startPos
    type(Vector2) :: endPos
    real :: thick, pad

    call draw_rectangle(int(x_px), int(y_px), int(w_px), int(h_px), cell_color_regular)

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

    call draw_rectangle(int(x_px), int(y_px), int(w_px), int(h_px), cell_color_regular)

    thick = w_px*0.2
    pad = w_px*0.2

    center%x = x_px + w_px/2
    center%y = y_px + h_px/2
    call draw_ring(center, (w_px - pad)/2 - thick, (w_px - pad)/2, 0.0, 360.0, 100, knot_color)
  end subroutine knot_cell
end program
