module Data.Function.Suffix
  ( (£)
  ) where

-- | Experimental operator, don't know if it's useful or composes well
-- but allows things like
--
-- @
-- doThis £ when $ that
-- @
infixr 2 £
(£) :: b -> (a -> b -> c) -> a -> c
(£) b = (b &) . flip

