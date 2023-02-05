type Bounded :: * -> Constraint
class Bounded a where
  minBound :: a
  maxBound :: a
  {-# MINIMAL minBound, maxBound #-}
        -- Defined in `GHC.Enum'
instance Bounded Word -- Defined in `GHC.Enum'
instance Bounded a => Bounded (Solo a) -- Defined in `GHC.Enum'
instance Bounded Ordering -- Defined in `GHC.Enum'
instance Bounded Int -- Defined in `GHC.Enum'
instance Bounded Char -- Defined in `GHC.Enum'
instance Bounded Bool -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k,
          Bounded l, Bounded m, Bounded n, Bounded o) =>
         Bounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k,
          Bounded l, Bounded m, Bounded n) =>
         Bounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k,
          Bounded l, Bounded m) =>
         Bounded (a, b, c, d, e, f, g, h, i, j, k, l, m)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k,
          Bounded l) =>
         Bounded (a, b, c, d, e, f, g, h, i, j, k, l)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j,
          Bounded k) =>
         Bounded (a, b, c, d, e, f, g, h, i, j, k)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j) =>
         Bounded (a, b, c, d, e, f, g, h, i, j)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i) =>
         Bounded (a, b, c, d, e, f, g, h, i)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h) =>
         Bounded (a, b, c, d, e, f, g, h)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g) =>
         Bounded (a, b, c, d, e, f, g)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f) =>
         Bounded (a, b, c, d, e, f)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) =>
         Bounded (a, b, c, d, e)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c, Bounded d) =>
         Bounded (a, b, c, d)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b, Bounded c) => Bounded (a, b, c)
  -- Defined in `GHC.Enum'
instance (Bounded a, Bounded b) => Bounded (a, b)
  -- Defined in `GHC.Enum'
instance Bounded () -- Defined in `GHC.Enum'