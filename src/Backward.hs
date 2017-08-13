module Backward
    ( deviceToClip
    , clipToEye
    , eyeToWorld
    , screenToDevice
    , rayFromMouse
    ) where

import           Linear.Matrix (M44, (!*))
import           Linear.Metric (normalize)
import           Linear.V3     (V3 (..))
import           Linear.V4     (V4 (..))
import           Mat

deviceToClip :: Float -> Float -> V4 Float
deviceToClip x y = V4 x y (-1) 1

clipToEye :: M44 Float -> V4 Float -> V4 Float
clipToEye persp point =
    let (V4 x y _ _) = Mat.inv44 persp !* point
    -- Unproject x and y.
    in V4 x y (-1) 0

eyeToWorld :: M44 Float -> V4 Float -> V3 Float
eyeToWorld view eye =
    let (V4 x y z _w) = Mat.inv44 view !* eye
        world = V3 x y z
    in world

width :: Float
width = 800

height :: Float
height = 600

screenToDevice :: Float -> Float -> (Float, Float)
screenToDevice mouseX mouseY =
    ((2 * mouseX) / width - 1, 1 - (2 * mouseY) / height)

rayFromMouse :: M44 Float -> M44 Float -> Float -> Float -> V3 Float
rayFromMouse persp view mouseX mouseY =
    let (x, y) = screenToDevice mouseX mouseY
        clipSpace = deviceToClip x y
        eyeSpace = clipToEye persp clipSpace
        worldSpace = eyeToWorld view eyeSpace
    in normalize worldSpace

{-}
    /* takes mouse position on screen and return ray in world coords */
    vec3 get_ray_from_mouse( float mouse_x, float mouse_y ) {
    	// screen space (viewport coordinates)
    	float x = ( 2.0f * mouse_x ) / g_gl_width - 1.0f;
    	float y = 1.0f - ( 2.0f * mouse_y ) / g_gl_height;
    	float z = 1.0f;
    	// normalised device space
    	vec3 ray_nds = vec3( x, y, z );
    	// clip space
    	vec4 ray_clip = vec4( ray_nds.v[0], ray_nds.v[1], -1.0, 1.0 );
    	// eye space
    	vec4 ray_eye = inverse( proj_mat ) * ray_clip;
    	ray_eye = vec4( ray_eye.v[0], ray_eye.v[1], -1.0, 0.0 );
    	// world space
    	vec3 ray_wor = vec3( inverse( view_mat ) * ray_eye );
    	// don't forget to normalise the vector at some point
    	ray_wor = normalise( ray_wor );
    	return ray_wor;
    }
-}
