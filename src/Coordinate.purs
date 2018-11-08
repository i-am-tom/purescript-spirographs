module Coordinate where

import Data.Monoid.Additive (Additive (..))
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Type.Data.RowList (RLProxy (..))
import Type.Data.Symbol (class IsSymbol, SProxy (..))
import Record (get, modify)
import Math (pow, sqrt)
import Prelude

class Coordinate (object :: Type) where
  transform :: (Number -> Number) -> (object -> object)
  fold :: forall m. Monoid m => (Number -> m) -> (object -> m)

class GCoordinate (row :: # Type) (list :: RowList) where
  transform' :: RLProxy list -> (Number -> Number) -> (Record row -> Record row)
  fold' :: forall m. Monoid m => RLProxy list -> (Number -> m) -> (Record row -> m)

instance gcoordinateNil :: GCoordinate row Nil where
  transform' _ _ = identity
  fold' _ _ _ = mempty

instance gcoordinateCons
    :: ( GCoordinate row tail
       , IsSymbol key
       , Row.Cons key Number xyz row
       )
    => GCoordinate row (Cons key Number tail) where
  transform' _ f record = modify key f (transform' tail f record)
    where
      key  = SProxy  :: SProxy  key
      tail = RLProxy :: RLProxy tail

  fold' _ f record = f (get key record) <> fold' tail f record
    where
      key  = SProxy  :: SProxy  key
      tail = RLProxy :: RLProxy tail

instance coordinateImpl
    :: (RowToList row list, GCoordinate row list)
    => Coordinate (Record row) where
  transform = transform' (RLProxy :: RLProxy list)
  fold      = fold'      (RLProxy :: RLProxy list)

offset :: forall row. Coordinate row => row -> Number
offset record = sqrt total
  where
    Additive total
      = fold (Additive <<< (_ `pow` 2.0)) record
