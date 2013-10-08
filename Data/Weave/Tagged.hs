{-|
Module      : Data.Weave.Tagged
Copyright   : (c) 2013 Craig Roche
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : stable
Portability : portable

-}

module Data.Weave.Tagged where

import Control.Arrow ( (***) )

import Control.Applicative

import Data.Traversable
import Data.Foldable ( Foldable, foldMap )
import Data.Monoid

import Data.Bifunctor
import Data.Foldable  ( toList )

import Data.Weave


newtype Tagged t a = Tagged { runTagged :: Weave a (t, a) }
    deriving Show

instance Functor (Tagged t) where
    fmap = fmapDefault

instance Bifunctor Tagged where
    bimap f g = Tagged . bimap g (f *** g) . runTagged

instance Monoid (Tagged t a) where
    mempty = Tagged mempty
    Tagged t `mappend` Tagged u = Tagged (t `mappend` u)

instance Traversable (Tagged t) where
    traverse f = fmap fromAssocList . (traverse.traversePair) f . toAssocList
      where
        traversePair f (b, a) = (,) b <$> f a

instance Foldable (Tagged t) where
    foldMap = foldMapDefault


-- * Constructors

-- | A value without a tag.
untag :: a -> Tagged t a
untag = Tagged . weave

-- | A value annotated with a tag.
tag :: t -> a -> Tagged t a
tag t a = Tagged $ pure (t, a)

-- | A list of untagged values
list :: [a] -> Tagged t a
list = foldMap untag

-- | A list of tag-value pairs
pairs :: [(t, a)] -> Tagged t a
pairs = foldMap (uncurry tag)

-- | Create a 'Tagged' from a list of values with optional tags
fromAssocList :: [(Maybe t, a)] -> Tagged t a
fromAssocList = mconcat . map f
  where
    f (Nothing, a) = untag a
    f (Just t,  a) = tag t a


-- * Destructors

-- | Reduce a 'Tagged' to a list, using a function on tagless values and a
-- function on tagged values.
tagged :: (a -> b) -> (t -> a -> b) -> Tagged t a -> [b]
tagged f g = flatten . bimap f (uncurry g) . runTagged

taggedA :: (Applicative f)
        => (a -> f b)
        -> (t -> a -> f b)
        -> Tagged t a
        -> f [b]
taggedA f g = sequenceA . tagged f g

-- | Reduce a 'Tagged' to a list by discarding all tags.
discardTags :: Tagged t a -> [a]
discardTags = tagged id (curry snd)

-- | Similar to 'discardTags', but discards the content instead of the tags
-- and uses the tags in their place.
joinTags :: Tagged a a -> [a]
joinTags = tagged id (curry fst)

-- | Convert a 'Tagged' into a list of tag-value pairs.
toAssocList :: Tagged t a -> [(Maybe t, a)]
toAssocList = tagged ((,) Nothing) ((,) . Just)

-- | Create an association list of tags and values, discarding the tagless
-- values.
listPairs :: Tagged t a -> [(t, a)]
listPairs = toList . runTagged


-- * Transformations

--liftWeave :: (Weave a (t, a) -> Weave b (s, b)) -> Tagged t a -> Tagged s b
--liftWeave f = Tagged . f . runTagged

-- | Map over all tags. This is a specialization of 'Data.Bifunctor.first'
mapTags :: (t -> u) -> Tagged t a -> Tagged u a
mapTags = Data.Bifunctor.first

reduceByTag :: (Applicative f) => (t -> f a) -> Tagged t a -> f [a]
reduceByTag f = sequenceA . tagged pure (const . f)

reduceByTagM :: (Monad m) => (t -> m a) -> Tagged t a -> m [a]
reduceByTagM f = Prelude.sequence . tagged return (const . f)

{-
traverseTaggeds :: (Applicative f) => (t -> f a) -> Tagged t a -> f [a]
traverseTaggeds f = transcribeA (f . fst)

traverseTaggedsM :: (Monad m) => (Name -> m a) -> Tagged a -> m a
traverseTaggedsM f = unwrapMonad . traverseTaggeds (WrapMonad . f)
-}
