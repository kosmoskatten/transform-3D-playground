module Forward
    ( toClipSpace
    , toScreenSpace
    , pointsInPerspective
    , transPipe
    ) where

import           Linear.Matrix (M44, (!*), (!*!))
import           Linear.V3     (V3 (..))
import           Linear.V4     (V4 (..), normalizePoint, point)

-- | Transform the point to clip space using the given matrix.
toClipSpace :: M44 Float -> M44 Float -> V3 Float -> V4 Float
toClipSpace view persp pos = (persp !*! view) !* point pos

toScreenSpace :: V3 Float -> (Float, Float)
toScreenSpace (V3 x y _) =
    ((x + 1) * width * 0.5, ((negate y) + 1) * height * 0.5)

-- | Generate a list of points, with the same x, y but with different z.
pointsInPerspective :: Float -> Float -> [Int] -> [V3 Float]
pointsInPerspective x y = map (V3 x y . fromIntegral)

-- | Run the complete transformation pipeline up to screen coordinates.
transPipe :: M44 Float -> M44 Float -> V3 Float -> (Float, Float)
transPipe view persp = toScreenSpace . normalizePoint . toClipSpace view persp

width :: Float
width = 800

height :: Float
height = 600
