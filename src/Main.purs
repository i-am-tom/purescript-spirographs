module Main where

import Color (hsv)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either (..))
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds (..))
import Effect (Effect)
import Effect.Console (error)
import Effect.Timer (setTimeout)
import FRP.Behavior (animate)
import FRP.Behavior.Time (seconds)
import Graphics.Canvas (getCanvasElementById, getContext2D, setCanvasDimensions)
import Graphics.Drawing (Drawing, circle, fillColor, filled, render)
import Math ((%), cos, pi, sin)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Fraction (Fraction, denominator, from, numerator, over)
import Prelude

-------------------------------------------------------------------------------
-- Helpful configuration constants.

canvasWidthAndHeight :: Number
canvasWidthAndHeight = 800.0

_cogToFrameRatio :: Maybe Fraction
_cogToFrameRatio = 3 `over` 10

_penOffsetToCogDiameterRatio :: Maybe Fraction
_penOffsetToCogDiameterRatio = 4 `over` 7

-------------------------------------------------------------------------------
-- The config object that we will pass around.

type Configuration
  = { cogToFrameRatio             :: Fraction
    , penOffsetToCogDiameterRatio :: Fraction
    }

-------------------------------------------------------------------------------
-- Coordinates.

newtype Coordinate 
  = Coordinate
      { x :: Number
      , y :: Number
      }

derive         instance eqCoordinate       :: Eq       Coordinate
derive newtype instance semiringCoordinate :: Semiring Coordinate
derive newtype instance ringCoordinate     :: Ring     Coordinate
derive newtype instance showCoordinate     :: Show     Coordinate

instance arbitraryCoordinate :: Arbitrary Coordinate where
  arbitrary = map Coordinate ({ x: _, y: _ } <$> arbitrary <*> arbitrary)

rotate :: Number -> Coordinate -> Coordinate
rotate angle (Coordinate { x, y })
  = Coordinate
      { x: cos angle * x - sin angle * y
      , y: sin angle * x + cos angle * y
      }

tau :: Number
tau = 2.0 * pi

-------------------------------------------------------------------------------
-- Convert from our coordinates to canvas pixels.

centreForCanvas :: Coordinate -> Coordinate
centreForCanvas (Coordinate { x, y })
  = Coordinate
      { x: halfDimension * x + halfDimension
      , y: halfDimension * y + halfDimension
      }
  where
    halfDimension = canvasWidthAndHeight / 2.0

-------------------------------------------------------------------------------
-- Calculate the position and rotation of the cog at a given time.

cogPosition
  :: Fraction
  -> Number
  -> { centre :: Coordinate, rotation :: Number }

cogPosition cogToFrameRatio time
  = { centre:   rotate time initial
    , rotation: time / from cogToFrameRatio
    }
  where
    initial = Coordinate { x: 0.0, y: 1.0 - from cogToFrameRatio }

-------------------------------------------------------------------------------
-- Calculate the position of the pen given the cog's position.

penPosition
  :: Configuration
  -> { centre :: Coordinate, rotation :: Number }
  -> Coordinate

penPosition config { centre, rotation }
  = centre + rotate rotation (Coordinate { x: 0.0, y: penOffset })
  where
    penOffset
      = from (config.cogToFrameRatio * config.penOffsetToCogDiameterRatio)


mark :: Configuration -> Seconds -> Drawing
mark config (Seconds time) = filled (fillColor colour) (circle x y 2.0)
  where
    _cogPosition
      = cogPosition config.cogToFrameRatio time

    Coordinate { x, y }
      = centreForCanvas
      $ penPosition config _cogPosition

    colour
      = hsv (time * 180.0 % 360.0) 0.8 0.8


app :: ExceptT String Effect Unit
app = do
  cogToFrameRatio <- case _cogToFrameRatio of
    Just ratio -> pure ratio
    Nothing    -> throwError "Invalid cog-to-frame ratio!"

  penOffsetToCogDiameterRatio <- case _penOffsetToCogDiameterRatio of
    Just ratio -> pure ratio
    Nothing    -> throwError "Invalid pen-offset-to-cog-diameter ratio!"

  canvas <- lift (getCanvasElementById "spirograph") >>= case _ of
    Just canvas -> pure canvas
    Nothing     -> throwError "Couldn't find canvas :("

  lift $ setCanvasDimensions canvas
    { width:  canvasWidthAndHeight
    , height: canvasWidthAndHeight
    }

  context <- lift (getContext2D canvas)

  let rollingRatio = cogToFrameRatio * penOffsetToCogDiameterRatio
      crossover    = toNumber $ lcm (numerator   rollingRatio)
                                    (denominator rollingRatio)

      completion   = 2000.0 * pi * crossover

  stopDrawing <- lift $ animate seconds \time -> do
    let config = { cogToFrameRatio, penOffsetToCogDiameterRatio }
    render context (mark config time)

  void $ lift $ setTimeout (ceil completion) stopDrawing

main :: Effect Unit
main = runExceptT app >>= case _ of
  Left information -> error information
  Right program    -> pure unit

