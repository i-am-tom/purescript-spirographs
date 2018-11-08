---
title: Spirographs
author: "@am\\_i\\_tom"
patat:
  incrementalLists: true

  images:
    backend: auto

  wrap: true

  margins:
    left:  10
    right: 10

  pandocExtensions:
    - emoji
    - patat_extensions
...

# :art: PureScript Spirographs :computer:

## Who am I? :sparkles:

- :tophat: Tom Harding

- :necktie: Habito (always hiring!)

- :speech_balloon: twitter.com/am\_i\_tom

- :seedling: github.com/i-am-tom

- :books: tomharding.me

# Spirographs :cyclone:

## How does it work? :bulb:

- Start with a (fixed) circle (as a perimeter).

- Pick a point on a smaller, rolling circle.

- Roll the second circle around the edge of the first.

- Trace the path of the chosen point.

- Repeat until Mum's off the phone.

# Risky Live Moment 1: Spirograph GIF :see_no_evil:

# PureScript

## In ASCII art? :space_invader:

                  .              'kKd'              
                 'okkkkOOOOOOx:. .,xXXd'            
             ..   .:llllllllllc'   .,xXXd'          
           .o0k'                     .xWNo.         
         .oKNk;. .;dxxxxxxxxxxo'   .c0NO:.          
       .oKNk;.   'cooooooooooc'  .c0NO:.            
      .oWWx.                     'dk:.              
       .c0NO:.   'lddddddddddo,.   .                
         .c0NO:. .,lddddddddddl'                    
           .cOx'                                    
             ..                                     

## In code? :sailboat:

```haskell
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello sailor!"
```

# Maths :100:

## Let's write a type! :pencil2:

```haskell
newtype Coordinate 
  = Coordinate
      { x :: Number
      , y :: Number
      }
```

## What can we do with it? :octopus:

```haskell
derive instance eqCoordinate
  :: Eq Coordinate

derive newtype instance semiringCoordinate
  :: Semiring Coordinate

derive newtype instance ringCoordinate
  :: Ring Coordinate

derive newtype instance showCoordinate
  :: Show Coordinate
```

## ... How do I write my _own_ code? :turtle:

```haskell
rotate
  :: Number -> Coordinate
  -> Coordinate

rotate angle (Coordinate { x, y })
  = Coordinate
      { x: cos angle * x - sin angle * y
      , y: sin angle * x + cos angle * y
      }
```

## Where's the rolling circle? :8ball:

```haskell
rollingCirclePosition
  :: Number -> Number
  -> Coordinate

rollingCirclePosition sizeRatio time
  = rotate time initial
  where
    initial = Coordinate
      { x: 0.0
      , y: 1.0 - sizeRatio
      }
```

## Which way up is it? :recycle:

```haskell
rollingCircleRotation
  :: Number -> Number
  -> Number

rollingCircleRotation sizeRatio time
  = -time / sizeRatio
```

## Ok, but where's the pen? :crystal_ball:

```haskell
penOffset
  :: Number -> Number -> Number
  -> Coordinate

penOffset sizeRatio offsetRatio rotation
  = rotate rotation
  $ Coordinate
      { x: 0.0
      , y: sizeRatio * offsetRatio
      }
```

# Drawing :camera:

## How do we draw a point on the canvas? :pushpin:

```haskell
mark
  :: Configuration -> Seconds
  -> Drawing

mark { sizeRatio, offsetRatio } (Seconds time)
  = filled (fillColor colour)
  $ circle x y 2.0
```

## Where did `x` and `y` come from? :telescope:

```haskell
  where
    rollingCentre
      = rollingCirclePosition sizeRatio time

    angle
      = rollingCircleRotation sizeRatio time

    Coordinate { x, y }
      = centreForCanvas
      $ rollingCentre
      + penPosition sizeRatio offsetRatio angle
```

## ... and `colour`? :rainbow:

```haskell
    colour
      = hsv (time * 180.0 % 360.0) 0.8 0.8
```

# Business logic :office:

## Dealing with the "real world" :skull:

```haskell
  canvas <- lift (getCanvasElementById "spirograph")
    >>= case _ of
      Just canvas -> pure canvas
      Nothing     -> throwError "No canvas :("

  lift $ setCanvasDimensions canvas
    { width: 400.0, height: 400.0 }

  context <- lift (getContext2D canvas)
```

# Risky Live Moment 2: The finished product :hear_no_evil:

## Draw me like one of your French curls :speedboat:

```haskell
  -- Current time as a stream   vvvvvvv
  stopDrawing <- lift $ animate seconds \time -> do
    let config = { sizeRatio, offsetRatio }
    render context (mark config time)
```

## A little more maths? :clap:

```haskell
  let crossover
        = toNumber
        $ numerator
        $ simplify sizeRatioAsFraction

      completion
        = 2000.0 * pi * crossover

  void
    $ lift
    $ setTimeout (ceil completion) stopDrawing
```

# Risky live moment 3: The even finisheder product :speak_no_evil:

# Could we have three dimensions :question:

# Yes :exclamation:

## Polymorphic accessors :mag:

```haskell
getX
  :: forall wrapper output anythingElse
   . Newtype wrapper { x :: output | anythingElse }
  => wrapper
  -> output

getX
  = _.x <<< unwrap
```

## Polymorphic coordinate operations :star:

```haskell
class Coordinate (object :: Type) where
  transform
    :: (Number -> Number)
    -> (object -> object)

  fold
    :: forall m. Monoid m
    => (Number -> m)
    -> (object -> m)
```

## Type-trickery :rocket:
 
```haskell
class GCoordinate
    (row  ::  # Type)
    (list :: RowList) where
  transform'
    :: RLProxy list -> (Number -> Number)
    -> (Record row -> Record row)

  fold'
    :: forall m. Monoid m
    => RLProxy list -> (Number -> m)
    -> (Record row -> m)
```

## The boring case :snail:

```haskell
instance gcoordinateNil
    :: GCoordinate row Nil where
  transform' _ _ = identity
  fold' _ _ _ = mempty
```

## The interesting case :rabbit:

```haskell
instance gcoordinateCons
    :: ( GCoordinate row tail, IsSymbol key
       , Row.Cons key Number xyz row )
    => GCoordinate row (Cons key Number tail) where
  transform' _ f record
    = modify (SProxy :: SProxy key) f
    $ transform' (RLProxy :: RLProxy tail) f record

  fold' _ f record
     = f (get (SProxy :: SProxy key) record)
    <> fold' (RLProxy :: RLProxy tail) f record
```

## Finally... :confetti_ball:

```haskell
instance coordinateImpl
    :: ( RowToList row list
       , GCoordinate row list )
    => Coordinate (Record row) where
  transform = transform' (RLProxy :: RLProxy list)
  fold      = fold'      (RLProxy :: RLProxy list)
```

## All this for what? :hammer:

```haskell
offset
  :: forall row. Coordinate row
  => row -> Number

offset record
  = sqrt total
  where
    folder x = Additive (x `pow` 2.0)
    Additive total = fold folder record
```

# Success :tada:

## ... Well, not quite :broken_heart:

- Floating point precision!

- `(a *  b     ) %  b      === 0`
- `(a *  2     ) %  2      === 0`
- `(a *  3     ) %  3      === 0`
- `(a *  π     ) %  π      === 0`
- `(a * (π / 4)) % (π / 4) === 1.2566`

- _show excuse.png_

- But, with a better number type, _yes_!

# "It is left as an exercise to the reader"

## Other exercises to the reader

- Stateful animation with `FRP.Behavior.fixB`.

- _purescript-super-circles_

- Continuous lines (_better laptops_).

- Interactive controls.

- Other types of ellipses to roll.

## Summary

- Present animations on a _newer laptop_.

- Simple canvas drawing with `purescript-drawing`.

- Simple animation with `purescript-behaviors`.

- More examples with `purescript-super-circles`.

- There was life before _the Internet_.

# Thank you!

## Questions?

> - :tophat: Tom Harding

> - :necktie: Habito (always hiring!)

> - :speech_balloon: twitter.com/am\_i\_tom

> - :seedling: github.com/i-am-tom

> - :books: tomharding.me

> - :crown: *github.com/jaspervdj/patat*
