{-|
Module      : Data.Weave
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : stable
Portability : portable

-}

module Data.Weave
(
  Weave
, fromEithers
, toEithers
-- * Constructors
, unitL
, unitR
, someL
, someR
-- * Destructors
, weave
, flatten
, transcribe
, transcribeA
-- * Transformations
, mapContext
-- * Other
, normalize
)
where

import Control.Arrow    ( (|||) )
import Data.Bifunctor
import Control.Applicative
import Data.Monoid
import Data.Either      ( lefts, rights )
import Data.Foldable    ( Foldable, foldMap )
import Data.Traversable ( Traversable, traverse )
import qualified Data.Traversable as Tr


-- | A list of interwoven @c@s and @a@s.
-- Intuitively, a @Weave c a@ is a container of @a@s with the values
-- interspersed within a context @c@.
-- Isomorphic to [Either c a].
newtype Weave c a = Weave { toEithers :: [Either c a] }
    deriving (Eq, Ord)

fromEithers :: [Either c a] -> Weave c a
fromEithers = Weave


instance (Show c, Show a) => Show (Weave c a) where
    showsPrec d w = showParen (d > 10) $
        showString "fromEithers " . shows (toEithers w)

instance Functor (Weave c) where
    fmap = Tr.fmapDefault

instance Bifunctor Weave where
    bimap l r = fromEithers . map (bimap l r) . toEithers

instance Foldable (Weave c) where
    foldMap = Tr.foldMapDefault

instance Traversable (Weave c) where
    traverse f (Weave es) = Weave <$> Tr.for es (pure . Left ||| fmap Right . f)

instance Monoid (Weave c a) where
    mempty = Weave []
    Weave xs `mappend` Weave ys = Weave (xs ++ ys)


unitL :: c -> Weave c a
unitL = Weave . (:[]) . Left

-- | Equivalent to 'pure'
unitR :: a -> Weave c a
unitR = Weave . (:[]) . Right

someL :: [c] -> Weave c a
someL = Weave . map Left

someR :: [a] -> Weave c a
someR = Weave . map Right


-- | A specialized version of 'first' from Data.Bifunctor
mapContext :: (c -> d) -> Weave c a -> Weave d a
mapContext = first

-- | Perform a right fold over a 'Weave'.
weave :: (c -> b -> b) -> (a -> b -> b) -> b -> Weave c a -> b
weave fc fa x = foldr (either fc fa) x . toEithers

flatten :: Weave a a -> [a]
flatten = weave (:) (:) []

transcribe :: (Monoid c) => (a -> c) -> Weave c a -> c
transcribe f = mconcat . map (either id f) . toEithers

transcribeA :: (Monoid c, Applicative f) => (a -> f c) -> Weave c a -> f c
transcribeA f = fmap mconcat . traverse (either pure f) . toEithers

normalize :: (Monoid c) => Weave c a -> Weave c a
normalize = fromEithers . foldMapLefts id . toEithers


-- Either helpers ----------

isLeft, isRight :: Either a b -> Bool
isLeft  = either (const True) (const False)
isRight = not . isLeft

mapAdjacentEithers :: ([a] -> [c]) -> ([b] -> [c]) -> [Either a b] -> [c]
mapAdjacentEithers _ _ [] = []
mapAdjacentEithers fl fr es@(e:_) = case e of
    Left  _ -> go isLeft  $ fl . lefts
    Right _ -> go isRight $ fr . rights
  where
    go p pull =
      let (xs, es') = span p es
      in pull xs ++ mapAdjacentEithers fl fr es'

segment :: [Either a b] -> [Either [a] [b]]
segment = mapAdjacentEithers ((:[]) . Left) ((:[]) . Right)

foldMapLefts :: (Monoid m) => (a -> m) -> [Either a b] -> [Either m b]
foldMapLefts f = mapAdjacentEithers ((:[]) . Left . foldMap f) (map Right)


test :: Weave Char Int
test = someL "ab" <> unitR 23 <> unitL 'c' <> unitR 12
