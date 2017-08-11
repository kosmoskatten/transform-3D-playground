module Mat
    ( makePerspective
    , makeView
    ) where

import           Linear.Matrix     (M44)
import qualified Linear.Projection as Proj
import           Linear.V3         (V3 (..))

-- | Construct a perspective matrix.
makePerspective :: M44 Float
makePerspective =
    Proj.perspective (pi / 4) (800 / 600) 0.1 100

-- | Construct a view matrix.
makeView :: M44 Float
makeView =
    Proj.lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)
