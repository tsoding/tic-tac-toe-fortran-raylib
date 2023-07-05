#include "macros.h"

module raylib
  use iso_c_binding, only: c_int32_t, c_char, c_int, c_bool, c_float, c_ptr
  implicit none
  type, bind(C) :: Vector2
     real(c_float) :: x, y
  end type Vector2

  ! typedef struct Texture {
  !   unsigned int id;        // OpenGL texture id
  !   int width;              // Texture base width
  !   int height;             // Texture base height
  !   int mipmaps;            // Mipmap levels, 1 by default
  !   int format;             // Data format (PixelFormat type)
  ! } Texture;
  type, bind(C) :: Texture
     integer(c_int) :: id, width, height, mipmap, format
  end type Texture

  ! typedef struct Font {
  !     int baseSize;           // Base size (default chars height)
  !     int glyphCount;         // Number of glyph characters
  !     int glyphPadding;       // Padding around the glyph characters
  !     Texture2D texture;      // Texture atlas containing the glyphs
  !     Rectangle *recs;        // Rectangles in texture for the glyphs
  !     GlyphInfo *glyphs;      // Glyphs info data
  ! } Font;
  type, bind(C) :: Font
     integer(c_int) :: baseSize, glyphCount, glyphPadding
     type(Texture) :: texture;
     type(c_ptr) :: recs, glyps;
  end type Font

  ! // Rectangle, 4 components
  ! typedef struct Rectangle {
  !     float x;                // Rectangle top-left corner position x
  !     float y;                // Rectangle top-left corner position y
  !     float width;            // Rectangle width
  !     float height;           // Rectangle height
  ! } Rectangle;
  type, bind(C) :: Rectangle
     real(c_float) :: x, y, width, height
  end type Rectangle

  ! TODO: use the Raylib colors
  integer(c_int32_t), parameter :: BLANK = 0
  integer(c_int32_t), parameter :: BLACK = color(z'FF000000')
  integer(c_int32_t), parameter :: WHITE = color(z'FFFFFFFF')
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
       import :: Vector2
       type(Vector2),value      :: startPos
       type(Vector2),value      :: endPos
       real(c_float),value      :: thick
       integer(c_int32_t),value :: color
     end subroutine draw_line_ex

     subroutine draw_ring(center,innerRadius,outerRadius,startAngle,endAngle,segments,color) bind(C, name="DrawRing")
       use iso_c_binding, only: c_float, c_int, c_int32_t
       import :: Vector2
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

     ! RLAPI void DrawText(const char *text, int posX, int posY, int fontSize, Color color);       // Draw text (using default font)
     subroutine draw_text(text,posX,posY,fontSize,color) bind(C, name="DrawText")
       use iso_c_binding, only: c_char, c_int, c_int32_t
       character(kind=c_char) :: text(*)
       integer(c_int),value :: posX, posY, fontSize
       integer(c_int32_t),value :: color
     end subroutine draw_text

     ! RLAPI Font LoadFont(const char *fileName);                                                  // Load font from file into GPU memory (VRAM)
     type(Font) function load_font(fileName) bind(C, name="LoadFont")
       use iso_c_binding, only: c_char
       import :: Font
       character(kind=c_char) :: fileName(*)
     end function load_font

     ! RLAPI void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint); // Draw text using font and additional parameters
     subroutine draw_text_ex(text_font, text, position, fontSize, spacing, tint) bind(C, name="DrawTextEx")
       use iso_c_binding, only: c_char, c_float, c_int32_t
       import :: Font, Vector2
       type(Font),value         :: text_font
       character(kind=c_char)   :: text(*)
       type(Vector2),value      :: position
       real(c_float),value      :: fontSize, spacing
       integer(c_int32_t),value :: tint
     end subroutine draw_text_ex

     !RLAPI Font LoadFontEx(const char *fileName, int fontSize, int *fontChars, int glyphCount);  // Load font from file with extended parameters, use NULL for fontChars and 0 for glyphCount to load the default character set
     type(Font) function load_font_ex(fileName, fontSize, fontChars, glyphCount) bind(C, name="LoadFontEx")
       use iso_c_binding, only: c_char, c_int, c_ptr
       import :: Font
       character(kind=c_char) :: fileName
       integer(c_int),value   :: fontSize
       type(c_ptr),value      :: fontChars
       integer(c_int),value   :: glyphCount
     end function load_font_ex

     ! RLAPI void DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color);              // Draw rectangle with rounded edges
     subroutine draw_rectangle_rounded(rec, roundness, segments, color) bind(C, name="DrawRectangleRounded")
       use iso_c_binding, only: c_float, c_int, c_int32_t
       import :: Rectangle
       type(Rectangle),value :: rec
       real(c_float),value :: roundness
       integer(c_int),value :: segments
       integer(c_int32_t),value :: color
     end subroutine draw_rectangle_rounded

     ! RLAPI Vector2 GetMousePosition(void);                         // Get mouse position XY
     type(Vector2) function get_mouse_position() bind(C, name="GetMousePosition")
       import :: Vector2
     end function get_mouse_position

     ! RLAPI bool CheckCollisionPointRec(Vector2 point, Rectangle rec);                                         // Check if point is inside rectangle
     logical(c_bool) function check_collision_point_rect(point, rec) bind(C, name="CheckCollisionPointRec")
       use iso_c_binding, only: c_bool
       import :: Vector2, Rectangle
       type(Vector2),value :: point
       type(Rectangle),value :: rec
     end function check_collision_point_rect

     ! RLAPI Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing);    // Measure string size for Font
     type(Vector2) function measure_text_ex(text_font, text, fontSize, spacing) bind(C, name="MeasureTextEx")
       use iso_c_binding, only: c_char, c_float
       import :: Vector2, Font
       type(Font),value       :: text_font
       character(kind=c_char) :: text(*)
       real(c_float),value    :: fontSize, spacing
     end function measure_text_ex

     ! RLAPI Color ColorBrightness(Color color, float factor);                     // Get color with brightness correction, brightness factor goes from -1.0f to 1.0f
     integer(c_int32_t) function color_brightness(color, factor) bind(C, name="ColorBrightness")
       use iso_c_binding, only: c_float, c_int32_t
       integer(c_int32_t), value :: color
       real(c_float), value :: factor
     end function color_brightness
  end interface
end module raylib
