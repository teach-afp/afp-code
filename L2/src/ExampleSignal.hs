-- | Examples for simple shapes.
module ExampleSignal where

import Signal  (Time, Signal, constS, timeS, ($$), mapT)
import Shape   (Shape, disc, square)
import Animate (animate)
import Render  (defaultWindow)

animateShape :: Signal Shape -> IO ()
animateShape sh = animate defaultWindow 0 endTime sh
  where endTime = 5

-- | It displays a shape on "odd times" and another one in "even times".
change :: Shape -> Shape -> Time -> Shape
change sh1 sh2 t
       | odd (floor t) = sh1
       | otherwise     = sh2

square_disc :: Signal Shape
square_disc = constS (change square disc) $$ timeS


to_zero :: Time -> Time
to_zero = const 0

always_disc :: Signal Shape
always_disc = mapT to_zero square_disc
