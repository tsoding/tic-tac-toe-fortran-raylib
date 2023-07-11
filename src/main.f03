#include "macros.h"

program main
  use iso_c_binding, only: c_int, c_int32_t, C_NULL_CHAR, C_NULL_PTR
  use raylib
  use game
  use ai
  use ui
  implicit none

  ! Measure units:
  ! - *_px - pixels
  ! - *_cl - cells
  ! - *_rl - relative (fraction of width, height, cell size, etc)
  integer(c_int),     parameter :: screen_width_px        = 16*80
  integer(c_int),     parameter :: screen_height_px       = 9*90
  integer(c_int),     parameter :: fps                    = 60
  integer(c_int32_t), parameter :: background_color       = color(z'FF181818')
  integer, parameter :: font_size = 69

  real    :: dt
  ! integer :: window_width_px, window_height_px
  real    :: board_x_px, board_y_px, board_boundary_width, board_boundary_height, board_size_px, cell_size_px

  integer,dimension(board_size_cl, board_size_cl) :: board

  integer :: current_player
  type(TLine) :: final_line
  integer :: state
  type(Font) :: game_font

  logical, dimension(2) :: ai_checkboxes

  enum, bind(C)
     enumerator :: STATE_GAME = 0
     enumerator :: STATE_WON
     enumerator :: STATE_TIE
  end enum

  ai_checkboxes(CELL_CROSS) = .false.
  ai_checkboxes(CELL_KNOTT) = .true.

  call restart_game()

  call set_config_flags(FLAG_WINDOW_RESIZABLE)
  call set_config_flags(FLAG_MSAA_4X_HINT)
  call init_window(screen_width_px, screen_height_px, "Fortran GOTY"//C_NULL_CHAR)
  call set_target_fps(fps)

  ! TODO: set the working directory to where the executable is located
  game_font = load_font_ex("./fonts/Alegreya-Regular.ttf"//C_NULL_CHAR, font_size, C_NULL_PTR, 0)
  call set_texture_filter(game_font%texture, TEXTURE_FILTER_BILINEAR)

  do while (.not. window_should_close())
     dt = get_frame_time()
     board_boundary_width  = get_render_width()*2/3
     board_boundary_height = get_render_height()

     if (board_boundary_width > board_boundary_height) then
        board_size_px = board_boundary_height
        board_x_px = real(board_boundary_width)/2 - board_size_px/2
        board_y_px = 0
     else
        board_size_px = board_boundary_width
        board_x_px = 0
        board_y_px = real(board_boundary_height)/2 - board_size_px/2
     end if

     board_x_px = board_x_px + board_size_px*board_margin_rl
     board_y_px = board_y_px + board_size_px*board_margin_rl
     board_size_px = board_size_px - board_size_px*board_margin_rl*2

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

     call render_ai_checkboxes(rectangle(board_boundary_width, 0, get_render_width() - board_boundary_width, board_boundary_height))
     call end_drawing()
  end do

contains
  subroutine render_ai_checkboxes(boundary)
    real,parameter :: checkbox_width_rl = 0.6
    real,parameter :: checkbox_height_rl = 0.10
    real,parameter :: checkbox_padding_rl = 0.05
    type(Rectangle),intent(in) :: boundary

    type(Rectangle) :: cross_boundary, knott_boundary
    real :: checkbox_height_px, checkbox_padding_px

    checkbox_height_px = boundary%height*checkbox_height_rl
    checkbox_padding_px = boundary%height*checkbox_padding_rl

    cross_boundary%width = boundary%width*checkbox_width_rl
    cross_boundary%height = checkbox_height_px
    cross_boundary%x = boundary%x + boundary%width/2 - cross_boundary%width/2
    cross_boundary%y = boundary%y + boundary%height/2 - checkbox_height_px - checkbox_padding_px*0.5

    knott_boundary%width = boundary%width*checkbox_width_rl
    knott_boundary%height = checkbox_height_px
    knott_boundary%x = boundary%x + boundary%width/2 - knott_boundary%width/2
    knott_boundary%y = boundary%y + boundary%height/2 + checkbox_padding_px*0.5

    call checkbox(cross_checkbox_id,cross_boundary,ai_checkboxes(CELL_CROSS))
    call checkbox(knott_checkbox_id,knott_boundary,ai_checkboxes(CELL_KNOTT))
  end subroutine render_ai_checkboxes

  subroutine render_tie_state()
    implicit none

    call render_board(board_x_px, board_y_px, board_size_px, board)

    if (restart_button(game_font, board_x_px, board_y_px, board_size_px)) then
      call restart_game()
    end if
  end subroutine render_tie_state

  subroutine render_won_state()
    implicit none

    call render_board(board_x_px, board_y_px, board_size_px, board)
    call strikethrough(final_line, board_x_px, board_y_px, board_size_px)

    if (restart_button(game_font, board_x_px, board_y_px, board_size_px)) then
       call restart_game()
    end if
  end subroutine render_won_state

  subroutine render_game_state()
    implicit none

    integer :: x_cl, y_cl, next_x_cl, next_y_cl

    if (ai_checkboxes(current_player)) then
       call render_board(board_x_px, board_y_px, board_size_px, board)

       if (.not. ai_next_move(board, current_player, next_x_cl, next_y_cl)) then
          board(next_x_cl, next_y_cl) = current_player
          if (player_won(board, CELL_CROSS, final_line)) then
             state = STATE_WON
             return
          end if
          if (player_won(board, CELL_KNOTT, final_line)) then
             state = STATE_WON
             return
          end if
          if (board_full(board)) then
             state = STATE_TIE
             return
          end if
          current_player = 3 - current_player
       else
          state = STATE_TIE
       end if
    else
       if (render_board_clickable(board_x_px, board_y_px, board_size_px, board, x_cl, y_cl)) then
          board(x_cl, y_cl) = current_player
          if (player_won(board, CELL_CROSS, final_line)) then
             state = STATE_WON
             return
          end if
          if (player_won(board, CELL_KNOTT, final_line)) then
             state = STATE_WON
             return
          end if
          if (board_full(board)) then
             state = STATE_TIE
             return
          end if
          current_player = 3 - current_player
       end if
    end if
  end subroutine render_game_state

  subroutine restart_game()
    board(:,:) = 0
    state = STATE_GAME
    current_player = CELL_CROSS
  end subroutine restart_game
end program

! # Roadmap
! - TODO: togglable AI (some sort of checkboxes on the side)
! - TODO: particle effects on placing the shapes
! - TODO: sound effects on placing the shapes and game over
! - TODO: customizable board size
! - TODO: accessibility: control via keyboard
