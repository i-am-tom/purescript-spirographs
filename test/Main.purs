module Test.Main where

import Data.Function (on)
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Math (floor, pi, round, sqrt)
import Prelude
import Test.QuickCheck (Result(Failed), (===))
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Fraction as F
import Main as M

to5dp :: Number -> Number
to5dp = (_ / 1e5) <<< round <<< (_ * 1e5)

nearlyEqual :: Number -> Number -> Result
nearlyEqual = (===) `on` to5dp

infix 2 nearlyEqual as =~=

bothNearlyEqual :: M.Coordinate -> M.Coordinate -> Result
bothNearlyEqual x@(M.Coordinate p) y@(M.Coordinate q)
  = M.Coordinate { x: to5dp p.x, y: to5dp p.y }
      === M.Coordinate { x: to5dp q.x, y: to5dp q.y }

infix 2 bothNearlyEqual as =~~=

main :: Effect Unit
main = run [consoleReporter] do
  describe "Fraction" do
    describe "simplify" do
      it "is fully simplified" $ quickCheck \x ->
        let fraction = F.simplify x

            numerator_   = F.numerator fraction
            denominator_ = F.denominator fraction

        in gcd numerator_ denominator_ === 1

      it "maintains proportions" $ quickCheck \x ->
        F.from x === F.from (F.simplify x)

  describe "Main" do
    describe "rotate" do
      let hypotenuse (M.Coordinate { x, y }) = sqrt (x * x + y * y)

      it "is identity for full turns" $ quickCheck \x t ->
        M.rotate (floor t * 2.0 * pi) x === x

      it "maintains offset" $ quickCheck \x r -> do
        hypotenuse (M.rotate r x) =~= hypotenuse x

      it "twice the hypotenuse away after a half turn" $ quickCheck \x t ->
        hypotenuse (M.rotate (pi + floor t * 2.0 * pi) x - x)
          =~= 2.0 * hypotenuse x

      it "negates" $ quickCheck \x r -> do
        M.rotate r (M.rotate (-r) x) =~~= x

      it "associates" $ quickCheck \x r1 r2 -> do
        M.rotate r1 (M.rotate r2 x) =~~= M.rotate (r1 + r2) x

    describe "centreForCanvas" do
      it "x >= 0.0" $ quickCheck \x ->
        M.centreForCanvas x # \(M.Coordinate p) ->
          p.x >= 0.0

      it "y >= 0.0" $ quickCheck \x ->
        M.centreForCanvas x # \(M.Coordinate p) ->
          p.y >= 0.0

      it "x <= widthAndHeight" $ quickCheck \x ->
        M.centreForCanvas x # \(M.Coordinate p) ->
          p.x <= M.canvasWidthAndHeight

      it "y <= widthAndHeight" $ quickCheck \x ->
        M.centreForCanvas x # \(M.Coordinate p) ->
          p.y <= M.canvasWidthAndHeight

    describe "cogPosition" do
      it "doesn't move after full turns" $ quickCheck \time -> do
        let turn  = floor (500.0 * time) * 2.0 * pi

        case M._cogToFrameRatio of
          Nothing ->
            Failed "Invalid cog-to-frame ratio!"

          Just r ->
            M.Coordinate { x: 0.0, y: 1.0 - F.from r }
              =~~= (M.cogPosition r turn).centre

      it "flips after half turns" $ quickCheck \time -> do
        let turn = pi + floor (500.0 * time) * 2.0 * pi

        case M._cogToFrameRatio of
          Nothing ->
            Failed "Invalid cog-to-frame ratio!"
        
          Just r ->
            M.Coordinate { x: 0.0, y: F.from r - 1.0 }
              =~~= (M.cogPosition r turn).centre

      it "maintains constant offset" $ quickCheck \angle coord -> do
        let hypotenuse (M.Coordinate { x, y }) = sqrt (x * x + y * y)

        hypotenuse (M.rotate angle coord) =~= hypotenuse coord

      it "associates" $ quickCheck \r1 r2 coord ->
        M.rotate r1 (M.rotate r2 coord) =~~= M.rotate (r1 + r2) coord

      it "negates" $ quickCheck \angle coord ->
        M.rotate angle (M.rotate (-angle) coord) =~~= coord

    describe "penPosition" do
      it "is identity for full turns" $ quickCheck \centre r -> do
        let rotation = floor (500.0 * r) * 2.0 * pi

        case M._cogToFrameRatio, M._penOffsetToCogDiameterRatio of
          Just cogToFrameRatio, Just penOffsetToCogDiameterRatio -> do
            let config = { cogToFrameRatio, penOffsetToCogDiameterRatio }
                ratio  = cogToFrameRatio * penOffsetToCogDiameterRatio

            M.penPosition config { centre, rotation } - centre
              =~~= M.Coordinate { x: 0.0, y: F.from ratio }

          _, _ ->
            Failed "Invalid ratios :("

      it "flips for half turns" $ quickCheck \centre r -> do
        let rotation = pi + floor (500.0 * r) * 2.0 * pi
        let hypotenuse (M.Coordinate { x, y }) = sqrt (x * x + y * y)

        case M._cogToFrameRatio, M._penOffsetToCogDiameterRatio of
          Just cogToFrameRatio, Just penOffsetToCogDiameterRatio -> do
            let config = { cogToFrameRatio, penOffsetToCogDiameterRatio }
                ratio  = cogToFrameRatio * penOffsetToCogDiameterRatio

                before = M.penPosition config { centre, rotation: 0.0 }
                after  = M.penPosition config { centre, rotation }

            hypotenuse (M.Coordinate { x: 0.0, y: F.from ratio })
              =~= hypotenuse (before - after) / 2.0

          _, _ ->
            Failed "Invalid ratios :("
