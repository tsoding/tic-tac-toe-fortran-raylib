module raymath
  use iso_c_binding, only: c_float
  implicit none

  type, bind(C) :: Vector2
     real(c_float) :: x, y
  end type Vector2

  interface
     ! RMAPI Vector2 Vector2Add(Vector2 v1, Vector2 v2)
     type(Vector2) function vector2_add(v1,v2) bind(C, name = "Vector2Add")
       import :: Vector2
       type(Vector2),value :: v1, v2
     end function vector2_add

     ! RMAPI Vector2 Vector2Scale(Vector2 v, float scale)
     type(Vector2) function vector2_scale(v,scale) bind(C, name = "Vector2Scale")
       use iso_c_binding, only: c_float
       import :: Vector2
       type(Vector2),value :: v
       real(c_float),value :: scale
     end function vector2_scale

     ! RMAPI Vector2 Vector2Lerp(Vector2 v1, Vector2 v2, float amount)
     type(Vector2) function vector2_lerp(v1, v2, amount) bind(C, name = "Vector2Lerp")
       use iso_c_binding, only: c_float
       import :: Vector2
       type(Vector2), value :: v1, v2
       real(c_float), value :: amount
     end function vector2_lerp
  end interface
end module raymath
