import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Maybe

import Pages
import Expr

canWidth, canHeight :: Num a => a
canWidth  = 300
canHeight = 300

-- Part H
-- Function to calculate all the points of the graph in terms of pixels
points :: Expr -> Double -> (Int,Int) -> [Point]
points exp_ scale (width,height) = zip x y
  where
    widthInt = fromIntegral width
    heightInt = fromIntegral height
    x = [0.0 .. widthInt]
    y = map (realToPix heightInt scale) yPixToReal
    xPixToReal = map (pixToReal widthInt scale) x
    yPixToReal = map (eval exp_) xPixToReal

-- Helper function that converts a pixel x-coordinate to a real x-coordinate
pixToReal :: Double -> Double -> Double -> Double
pixToReal width scale x = (2*x - width)/2 * scale

-- Helper function that converts a real y-coordinate to a pixel y-coordinate
realToPix :: Double -> Double -> Double -> Double
realToPix width scale y = width/2 -y/scale

-- Part I
-- Function that reads the expression from the given input
-- element and draws the graph on the given canvas
readAndDraw :: Elem -> Elem -> Canvas -> Double -> IO ()
readAndDraw input display can scale = do
      set display [ prop "value" =: (show scale) ]
      readAndDraw' input can scale

-- Helper function that checks whether it is an expression or not
-- uses helper function checkExpr, then draws the expression if it exists
readAndDraw' :: Elem -> Canvas -> Double -> IO ()
readAndDraw' exp_ can scale = do
      expr <- checkExpr exp_
      if isJust expr
      then display $ points (fromJust expr) scale canDimensions
      else error "Expression is invalid"
  where display p = render can (stroke (path p))
        canDimensions = (canHeight,canWidth)

-- Helper function that checks whether it is an expression or not
-- uses getValue to return an expression or nothing if it is not valid
checkExpr :: Elem -> IO (Maybe Expr)
checkExpr expr = do
               isExpr <- getValue expr
               if isJust isExpr
               then return $ readExpr $ fromJust isExpr
               else return $ Nothing

-- Part J
-- Function that zooms in or zooms out the canvas
zooming :: Elem -> Elem -> Canvas -> Double -> IO ()
zooming input display can step =
    do
      expr <- getValue display
      let scale = (read (fromJust expr)) + step :: Double
      readAndDraw input display can scale

-- Part K

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    display <- mkInput 20 "0"                -- The display input
    zoomIn  <- mkButton "Zoom +"                  -- The zoom in button
    zoomOut <- mkButton "Zoom -"                  -- The zoom in button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw,zoomIn,zoomOut]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Scale (1 pixel corresponds to 0.04 in the floating point world)
    let scale = 0.04

    -- Step definition for the zoom functionality
    let step = 0.01

    -- Interaction
    Just can <- fromElem canvas
    onEvent draw Click $ \ _ -> readAndDraw input display can scale
    onEvent input Change $ \ _ -> readAndDraw input display can scale
    onEvent zoomIn Click $ \_  -> zooming input display can (-step)
    onEvent zoomOut Click $ \_  -> zooming input display can step
