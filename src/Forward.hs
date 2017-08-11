module Forward
    ( toClipSpace
    , pointsInPerspective
    , transPipe
    ) where

import           Linear.Matrix (M44, (!*))
import           Linear.V3     (V3 (..))
import           Linear.V4     (V4 (..), normalizePoint, point)

-- | Transform the point to clip space using the given matrix.
toClipSpace :: M44 Float -> V3 Float -> V4 Float
toClipSpace mvp pos = mvp !* point pos

-- | Generate a list of points, with the same x, y but with different z.
pointsInPerspective :: Float -> Float -> [Int] -> [V3 Float]
pointsInPerspective x y = map (V3 x y . fromIntegral)

-- | Run the complete transformation pipeline up to normalized device coords.
transPipe :: M44 Float -> V3 Float -> V3 Float
transPipe mvp = normalizePoint . toClipSpace mvp
