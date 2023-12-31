module ui
  use iso_c_binding, only: c_null_char, c_int32_t
  use raylib
  use game
  implicit none

  type :: Square
     real, dimension(2) :: pos
     real               :: size
  end type Square

  type :: Button_Style
     integer(c_int32_t) :: color
     real :: hover, hold
  end type Button_Style

  enum, bind(C)
     enumerator :: BUTTON_UNPRESSED = 0
     enumerator :: BUTTON_HOVER
     enumerator :: BUTTON_HOLD
  end enum

  integer(c_int32_t), parameter :: background_color        = int(z'FF181818', c_int32_t)
  integer(c_int32_t), parameter :: cell_color              = int(z'FF252525', c_int32_t)
  integer(c_int32_t), parameter :: knot_color              = int(z'FF3030F0', c_int32_t)
  integer(c_int32_t), parameter :: cross_color             = int(z'FF30F060', c_int32_t)
  integer(c_int32_t), parameter :: shape_colors(2)         = [cross_color, knot_color]
  integer(c_int32_t), parameter :: restart_button_color    = int(z'FFEEEEEE', c_int32_t)
  integer(c_int32_t), parameter :: strikethrough_color     = int(z'FFEEEEEE', c_int32_t)
  real,               parameter :: board_padding_rl        = 0.03
  real,               parameter :: board_margin_rl         = 0.10
  real,               parameter :: restart_button_width_rl = 0.3
  type(Button_Style), parameter :: cell_button_style = Button_Style( &
       color = cell_color, &
       hover = 0.10, &
       hold = 0.15)
  type(Button_Style), parameter :: restart_button_style = Button_Style( &
       color = restart_button_color, &
       hover = -0.10, &
       hold = -0.15)
  real,               parameter :: checkbox_line_thickness_px = 10.0
  integer(c_int32_t), parameter :: checkbox_color             = restart_button_color
  integer,            parameter :: restart_button_id          = board_size_cl*board_size_cl + 1
  integer,            parameter :: cross_checkbox_id          = restart_button_id + 1
  integer,            parameter :: knott_checkbox_id          = cross_checkbox_id + 1
  integer(c_int),     parameter :: screen_factor              = 120
  integer(c_int),     parameter :: screen_width_px            = 16*screen_factor
  integer(c_int),     parameter :: screen_height_px           = 9*screen_factor

  integer :: active_button_id = 0
  real :: screen_scale = 1, screen_offset_x = 0, screen_offset_y = 0

contains
  subroutine begin_screen_fitting()
    type(Camera2D) :: camera

    screen_scale = real(get_render_width())/real(screen_width_px)
    if (get_render_height() < screen_height_px*screen_scale) then
       screen_scale = real(get_render_height())/real(screen_height_px)
    end if
    screen_offset_x = get_render_width()/2 - screen_width_px*screen_scale/2
    screen_offset_y = get_render_height()/2 - screen_height_px*screen_scale/2
    call set_mouse_offset(int(-screen_offset_x), int(-screen_offset_y))
    call set_mouse_scale(1.0/screen_scale, 1.0/screen_scale)

    camera%target = Vector2([0, 0])
    camera%rotation = 0
    camera%offset = Vector2([screen_offset_x, screen_offset_y])
    camera%zoom = screen_scale
    call begin_mode_2d(camera)
  end subroutine begin_screen_fitting

  subroutine end_screen_fitting()
    call set_mouse_offset(0, 0)
    call set_mouse_scale(1.0, 1.0)
    call end_mode_2d()
  end subroutine end_screen_fitting

  function restart_button(button_font, board_x_px, board_y_px, board_size_px) result(clicked)
    type(Font), intent(in) :: button_font
    real,       intent(in) :: board_x_px, board_y_px, board_size_px
    logical :: clicked

    type(Vector2) :: text_pos, text_size
    type(Rectangle) :: rec

    rec%width = board_size_px*restart_button_width_rl
    rec%height = rec%width*0.4
    rec%x = board_x_px + board_size_px/2 - rec%width/2
    rec%y = board_y_px + board_size_px/2 - rec%height/2

    clicked = button(restart_button_id, rec, restart_button_style)

    text_size = measure_text_ex(button_font, "Restart"//C_NULL_CHAR, rec%height*0.45, 0.0)
    text_pos = Vector2([rec%x, rec%y] + [rec%width, rec%height]/2 - text_size%array/2)
    call draw_text_ex(button_font, "Restart"//C_NULL_CHAR, text_pos, rec%height*0.45, 0.0, BLACK)
  end function restart_button

  subroutine empty_cell(x_px, y_px, s_px, color)
    real,               intent(in) :: x_px, y_px, s_px
    integer(c_int32_t), intent(in) :: color

    call draw_rectangle_rounded(Rectangle(x_px, y_px, s_px, s_px), 0.1, 10, color)
  end subroutine empty_cell

  function button_logic(id, boundary, state) result(clicked)
    integer,         intent(in)  :: id
    type(Rectangle), intent(in)  :: boundary
    integer,         intent(out) :: state
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
       ! TODO: handle the situation when the active button was not rendered on mouse release
       ! If on mouse release the active button was not rendering, it may softlock the whole system
       state = BUTTON_UNPRESSED
    end if
  end function button_logic

  subroutine fit_square(boundary,x_px,y_px,s_px)
    type(Rectangle), intent(in)  :: boundary
    real,            intent(out) :: x_px, y_px, s_px

    if (boundary%width < boundary%height) then
       s_px = boundary%width
       x_px = boundary%x
       y_px = boundary%y + boundary%height/2 - boundary%width/2
    else
       s_px = boundary%height
       x_px = boundary%x + boundary%width/2 - boundary%height/2
       y_px = boundary%y
    end if
  end subroutine fit_square

  subroutine tick(x, y, s, color)
    real,               intent(in) :: x, y, s
    integer(c_int32_t), intent(in) :: color
    real                           :: thick
    type(Vector2)                  :: p1, p2, p3
    thick = s*0.16
    p1 = Vector2([x + s/4, y + s/2])
    p2 = Vector2([x + s/2, y + s*2/3])
    p3 = Vector2([x + s/2 + s/4, y + s/4])
    call draw_line_ex(p1, p2, thick, color)
    call draw_line_ex(p2, p3, thick, color)
    ! call draw_circle_v(p1, thick/2, color)
    call draw_circle_v(p2, thick/2, color)
    ! call draw_circle_v(p3, thick/2, color)
  end subroutine tick

  subroutine checkbox(id,shape,boundary,state)
    integer,         intent(in)    :: id, shape
    type(Rectangle), intent(in)    :: boundary
    logical,         intent(inout) :: state

    integer :: button_state
    real :: shape_x, shape_y, shape_s
    real :: tick_x, tick_y, tick_s

    integer(c_int32_t) :: color

    tick_x = boundary%x
    tick_y = boundary%y
    if (boundary%width < boundary%height) then
       tick_s = boundary%width
       call fit_square(Rectangle(tick_x, tick_y + tick_s, boundary%width, boundary%height - tick_s), shape_x, shape_y, shape_s)
    else
       tick_s = boundary%height
       call fit_square(Rectangle(tick_x + tick_s, tick_y, boundary%width - tick_s, boundary%height), shape_x, shape_y, shape_s)
    end if

    if (button_logic(id, boundary, button_state)) then
       state = .not. state
    end if

    select case (button_state)
    case (BUTTON_UNPRESSED)
       color = restart_button_color
    case (BUTTON_HOVER)
       color = color_brightness(restart_button_color, restart_button_style%hover)
    case (BUTTON_HOLD)
       color = color_brightness(restart_button_color, restart_button_style%hold)
    end select

    if (state) then
       call draw_rectangle_rounded(Rectangle(tick_x, tick_y, tick_s, tick_s), 0.1, 10, color)
       ! call draw_rectangle_rec(Rectangle(tick_x, tick_y, tick_s, tick_s), color)
       call tick(tick_x, tick_y, tick_s, background_color)
    else
       call draw_rectangle_rounded(Rectangle(tick_x, tick_y, tick_s, tick_s), 0.1, 10, color)
       call draw_rectangle_rounded( &
            Rectangle(tick_x + checkbox_line_thickness_px, &
                      tick_y + checkbox_line_thickness_px, &
                      tick_s - checkbox_line_thickness_px*2, &
                      tick_s - checkbox_line_thickness_px*2), &
            0.1, 10, background_color)
       ! call draw_rectangle_lines_ex(Rectangle(tick_x, tick_y, tick_s, tick_s), checkbox_line_thickness_px, color)
    end if

    select case (shape)
    case (CELL_CROSS)
       call cross(shape_x, shape_y, shape_s)
    case (CELL_KNOTT)
       call knot(shape_x, shape_y, shape_s)
    end select
  end subroutine checkbox

  function button(id,boundary,style) result(clicked)
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
    integer, intent(in) :: id
    real,    intent(in) :: x_px, y_px, s_px
    logical :: clicked
    clicked = button(id, rectangle(x_px, y_px, s_px, s_px), cell_button_style)
  end function empty_cell_clickable

  subroutine knot(x_px,y_px,s_px)
    real, intent(in) :: x_px, y_px, s_px

    type(Vector2) :: center
    real :: thick, pad

    thick = s_px*0.2
    pad = s_px*0.2

    center = Vector2([x_px, y_px] + s_px/2)
    call draw_ring(center, (s_px - pad)/2 - thick, (s_px - pad)/2, 0.0, 360.0, 100, knot_color)
  end subroutine knot

  subroutine knot_cell(x_px,y_px,s_px)
    real, intent(in) :: x_px, y_px, s_px

    call empty_cell(x_px, y_px, s_px, cell_color)
    call knot(x_px, y_px, s_px)
  end subroutine knot_cell

  subroutine cross(x_px,y_px,s_px)
    real, intent(in) :: x_px, y_px, s_px

    type(Vector2) :: startPos
    type(Vector2) :: endPos
    real :: thick, pad

    thick = s_px*0.2
    pad = s_px*0.2

    startPos   = Vector2([x_px, y_px] + pad)
    endPos     = Vector2([x_px, y_px] + s_px - pad)
    call draw_line_ex(startPos, endPos, thick, cross_color)

    startPos = Vector2([x_px + s_px - pad, y_px + pad])
    endPos   = Vector2([x_px + pad, y_px + s_px - pad])
    call draw_line_ex(startPos, endPos, thick, cross_color)
  end subroutine cross

  subroutine cross_cell(x_px,y_px,s_px)
    real, intent(in) :: x_px, y_px, s_px
    call empty_cell(x_px, y_px, s_px, cell_color)
    call cross(x_px, y_px, s_px)
  end subroutine cross_cell

  subroutine render_board(board_x_px, board_y_px, board_size_px, board)
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

  subroutine map_tline_on_screen(line, board_square, start_px, end_px)
    type(TLine),  intent(in)  :: line
    type(Square), intent(in)  :: board_square
    real,         intent(out) :: start_px(2), end_px(2)
    integer                   :: start_cl(2), end_cl(2)

    real :: cell_size_px

    cell_size_px = board_square%size/board_size_cl

    start_cl = line%p
    end_cl   = line%p + line%d
    start_px = board_square%pos + (start_cl-1)*cell_size_px + cell_size_px/2
    end_px   = board_square%pos + (end_cl-1)*cell_size_px   + cell_size_px/2
  end subroutine map_tline_on_screen

  subroutine strikethrough(final_line, board_square)
    type(TLine),        intent(in) :: final_line
    type(Square),       intent(in) :: board_square

    real :: thick, cell_size_px
    real,dimension(2) :: startPos, endPos

    cell_size_px = board_square%size/board_size_cl

    thick = cell_size_px*0.1
    call map_tline_on_screen(final_line, board_square, startPos, endPos)
    call draw_line_ex(Vector2(startPos), Vector2(endPos), thick, strikethrough_color)
  end subroutine strikethrough
end module ui
