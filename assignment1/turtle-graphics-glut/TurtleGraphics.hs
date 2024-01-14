-- | A graphical run function for the turtle DSL
--
-- See tutorial starting at: https://www.haskell.org/haskellwiki/OpenGLTutorial1
module TurtleGraphics (runGraphical) where

import qualified Graphics.Rendering.OpenGL.GL  as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLUT              as GLUT
import           Graphics.UI.GLUT                      (GLint, GLdouble)

import Turtle

runGraphical :: Program -> IO ()
runGraphical p = do
    (_progName, _args) <- GLUT.getArgsAndInitialize
    window <- GLUT.createWindow "Turtle!"
    GLUT.reshapeCallback GLUT.$= Just setDisplay
    GLUT.displayCallback GLUT.$= display
    setDisplay =<< GLUT.get GLUT.windowSize
    GLUT.mainLoop
  where
    display :: GLUT.DisplayCallback
    display = do
        GLUT.clear [GLUT.ColorBuffer]
        setColor 1 1 1
        triangleFan [(0,0),(0,300),(300,300),(300,0)]
        setColor 1 0 0
        line  (100, 200) (200, 100)
        setColor 0 0.75 0
        line  (100, 100) (200, 200)
        setColor 0 0 0.5
        line  (150, 100) (150, 200)
        setColor 0 0 0
        line  (100, 150) (200, 150)
        GLUT.flush

setColor :: Double -> Double -> Double -> IO ()
setColor r g b = GL.color $ GLUT.Color3 (rtf r) (rtf g) (rtf b)
  where rtf = realToFrac :: Double -> GLdouble

conv :: (Integral a,Num b) => a -> b
conv = fromIntegral . toInteger

setDisplay :: GLUT.Size -> IO ()
setDisplay (GLUT.Size w h) = do
    GLUT.viewport GLUT.$= (GLUT.Position 0 0,GLUT.Size w h)
    GL.loadIdentity
    GLU.ortho2D 0 (conv w) (conv h) 0

triangleFan :: [(Int,Int)] -> IO ()
triangleFan xys = GLUT.renderPrimitive GLUT.TriangleFan $ sequence_
    [ GLUT.vertex $ GLUT.Vertex3 (conv x) (conv y) (0 :: GLint)
    | (x,y) <- xys
    ]

line :: (Int,Int) -> (Int,Int) -> IO ()
line (x0,y0) (x1,y1) = GLUT.renderPrimitive GLUT.Lines $ do
    GLUT.vertex $ GLUT.Vertex3 (conv x0) (conv y0) (0 :: GLint)
    GLUT.vertex $ GLUT.Vertex3 (conv x1) (conv y1) (0 :: GLint)

