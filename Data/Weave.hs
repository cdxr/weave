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
, unit
, context
-- * Destructors
, weave
, transcribe
, transcribeA
-- * Other
, normalize
)
where

import Control.Arrow
import Control.Applicative
import Data.Monoid
import Data.Foldable ( Foldable, foldMap )
import Data.Traversable ( traverse, for, fmapDefault, foldMapDefault )
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

instance Functor (Weave t) where
    fmap = fmapDefault

instance Foldable (Weave t) where
    foldMap = foldMapDefault

instance Tr.Traversable (Weave t) where
    traverse f (Weave es) = Weave <$> for es (pure . Left ||| fmap Right . f)

instance Monoid (Weave t a) where
    mempty = Weave []
    Weave xs `mappend` Weave ys = Weave (xs ++ ys)


unit :: a -> Weave t a
unit = Weave . (:[]) . Right

context :: [t] -> Weave t a
context = Weave . map Left


-- | Perform a right fold over a 'Weave'.
weave :: (c -> b -> b) -> (a -> b -> b) -> b -> Weave c a -> b
weave fc fa x = foldr (either fc fa) x . toEithers



normalize :: (Monoid t) => Weave t a -> Weave t a
normalize = fromEithers . combineLefts . toEithers
  where
    isLeft = either (const True) (const False)

    combineLefts es =
      case es of
          [] -> []
          (Right x:xs) -> Right x : combineLefts xs
          _ ->
            case span isLeft es of
                ([r], xs) -> r : combineLefts xs
                (rs,  xs) -> Left (mconcat (map fromLeft rs)) : combineLefts xs
    fromLeft (Left a) = a
    fromLeft (Right  _) = undefined


transcribe :: (Monoid t) => (a -> t) -> Weave t a -> t
transcribe f = mconcat . map (either id f) . toEithers

transcribeA :: (Monoid t, Applicative f) => (a -> f t) -> Weave t a -> f t
transcribeA f = fmap mconcat . traverse (either pure f) . toEithers

test :: Weave Char Int
test = context "ab" <> unit 23 <> context "c" <> unit 12
