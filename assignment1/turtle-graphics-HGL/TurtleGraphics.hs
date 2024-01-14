-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import qualified Graphics.HGL as HGL

import Turtle

runGraphical :: Program -> IO ()
runGraphical p = HGL.runGraphics $ do
    w <- HGL.openWindowEx "Turtle!" Nothing (300, 300) HGL.DoubleBuffered (Just 1000)
    HGL.drawInWindow w (HGL.polygon [(0,0),(0,300),(300,300),(300,0)])
    onTick w [ HGL.withColor HGL.Red   $ HGL.line  (100, 200) (200, 100)
             , HGL.withColor HGL.Green $ HGL.line  (100, 100) (200, 200)
             , HGL.withColor HGL.Blue  $ HGL.line  (150, 100) (150, 200)
             , HGL.withColor HGL.Black $ HGL.line  (100, 150) (200, 150)
             ]
    HGL.getKey w >> return ()


onTick :: HGL.Window -> [HGL.Graphic] -> IO ()
onTick w []      = return ()
onTick w (x:xs)  = do
  HGL.getWindowTick w
  HGL.drawInWindow w x
  onTick w xs
