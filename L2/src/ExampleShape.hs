-- | Examples for simple shapes.
module ExampleShape where

import Signal  (constS)
import Shape   (Shape, disc, square, difference,
                scale, translate, transform, vec, rotate, invert,
                intersect, union)
import Animate (animate)
import Render  (defaultWindow)
import Matrix

{- |
  We run the example with no animation (so, we wrap the
  shape using the constant signal)
-}
showShape :: Shape -> IO ()
showShape = (animate defaultWindow 0 0.1) . constS

-- | Playing around with shapes (show disc and square first)
ex_1 = invert disc
ex_2 = intersect (invert disc) square

-- | Zoom in
zoom_in n shape = transform m shape
    where m = matrix (n/100+1) 0
                     0         (n/100+1)

-- | Zoom out
zoom_out n shape = transform m shape
    where m = matrix (-n/100+1) 0
                     0          (-n/100+1)

-- | Zoom out 50%
ex_3 = zoom_in 50 ex_2

-- | Move to the left
ex_4 = translate (vec (0.5) 0) square

-- | Rotate clock-wise 30 degrees (using counter clock-wise matrix)
ex_5 = transform (m (pi/10)) square
    where m alpha = matrix (cos alpha)    (sin alpha)
                       (-(sin alpha)) (cos alpha)

-- | Rotate counter clock-wise 30 degrees (using clock-wise matrix)
ex_6 = transform (m (pi/10) ) square
    where m alpha = matrix (cos alpha) (-(sin alpha))
                       (sin alpha) (cos alpha)
