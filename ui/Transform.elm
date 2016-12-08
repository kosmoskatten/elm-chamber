module Transform exposing (pitch, yaw, roll, moveTo)

import Math.Matrix4 exposing (Mat4, makeRotate, makeTranslate)
import Math.Vector3 exposing (Vec3, vec3)


-- Make a pitch matrix - rotate theta radians on the x-axis.


pitch : Float -> Mat4
pitch theta =
    makeRotate theta <| vec3 1 0 0



-- Make a yaw matrix - rotate theta radians on the y-axis.


yaw : Float -> Mat4
yaw theta =
    makeRotate theta <| vec3 0 1 0



-- Make a roll matrix - rotate theta radians on the z-axis.


roll : Float -> Mat4
roll theta =
    makeRotate theta <| vec3 0 0 1



-- Make a translation to the given coordinate (just an alias for makeTranslate)


moveTo : Vec3 -> Mat4
moveTo =
    makeTranslate
