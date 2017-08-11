module Backward
    ( clipToModel
    ) where

import           Linear.Matrix (M44, (!*), (!*!))
import           Linear.V4     (V4 (..))
import           Mat

clipToModel :: M44 Float -> M44 Float -> V4 Float -> V4 Float
clipToModel view persp point =
    let view' = Mat.inv44 view
        persp' = Mat.inv44 persp
    in (view' !*! persp') !* point
