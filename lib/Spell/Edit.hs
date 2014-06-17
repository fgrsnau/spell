module Spell.Edit where

import           Control.Arrow ((&&&))
import           Control.Monad (guard)

import           Data.ListLike.Instances ()
import           Data.Maybe (fromJust, isJust, listToMaybe)
import qualified Data.PQueue.Prio.Min as PMin
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Trie (Trie, cut, branches, end, expandPaths, populate, value)
import           Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V


data Penalties a p = Penalties
    { penaltyInsertion    :: Maybe a -> a -> p
    , penaltyDeletion     :: Maybe a -> a -> p
    , penaltySubstitution ::       a -> a -> p
    , penaltyReversal     ::       a -> a -> p
    }


defaultPenalties :: Eq a => Penalties a Int
defaultPenalties = Penalties
    { penaltyInsertion    = \_ _ -> 1
    , penaltyDeletion     = \_ _ -> 1
    , penaltySubstitution = \x y -> if x == y then 0 else 1
    , penaltyReversal     = \_ _ -> 1
    }

calculateEdit :: (Num p, Ord p, Unbox p)
                 => Penalties Char p -> Text -> [Char] -> [Vector p] -> Vector p
calculateEdit p r = f
  where
    f [] [] = V.scanl (+) 0 . V.map (penaltyInsertion p Nothing) . V.fromList $ T.unpack r
    f ks vs = V.constructN (T.length r + 1) (f' ks vs)

    f' (k:ks) (v:vs) v'
      | V.null v' = v V.! i + penaltyDeletion p (listToMaybe ks) k
      | isJust reversal = fromJust reversal
      | otherwise = minimum [ v  V.!   i   + penaltyDeletion p (listToMaybe ks) k
                            , v' V.! (i-1) + penaltyInsertion p (Just k) (r `T.index` (i-1))
                            , v  V.! (i-1) + penaltySubstitution p k (r `T.index` (i-1))
                            ]
      where
        i = V.length v'

        reversal = do
          let currentSuggestion = k
          lastSuggestion <- listToMaybe ks
          let currentChar = r `T.index` (i-1)
          lastChar <- do
            guard $ i - 2 >= 0
            return $ r `T.index` (i - 2)
          correctVector <- listToMaybe vs
          guard (currentSuggestion == lastChar)
          guard (lastSuggestion == currentChar)
          return $ correctVector V.! (i-2) + penaltyReversal p lastSuggestion currentSuggestion

    f' _ _ _ = error "This function should never get called."


shrinkMatrices :: (Ord p, Unbox p) => Trie Char (Vector p) -> Trie Char (p, p)
shrinkMatrices = fmap (V.last &&& V.minimum)

searchBestEdits :: (Num p, Ord p) => Trie Char (Text, (p, p)) -> [Text]
searchBestEdits trie = processQueue (finished, queue)
  where
    finished = PMin.empty
    queue    = PMin.singleton 0 trie

    processQueue (f, q)
      | PMin.null q                    = map snd $ PMin.toAscList f
      | not (PMin.null f) && f'' < q'' = path : processQueue (f', q)
      | otherwise                      = processQueue $ processNext (f, q)
      where
        Just ((f'', path), f') = PMin.minViewWithKey f
        (q'', _) = PMin.findMin q

    processNext (f, q)
      | end t     = (f', q'')
      | otherwise = (f,  q'')
      where
        Just (t, q') = PMin.minView q
        (path, (last', _)) = value t
        f' = PMin.insert last' path f
        q'' = q' `PMin.union` PMin.fromList (map (processBranches .snd) $ branches t)

    processBranches t = let (_, (_, min')) = value t in (min', t)

bestEdits :: (Num p, Ord p, Unbox p)
             => Penalties Char p -> Maybe p -> Text -> Trie Char () -> [Text]
bestEdits p c r = searchBestEdits
                . expandPaths
                . maybe id doCut c
                . shrinkMatrices
                . populate (calculateEdit p r)
  where
    doCut n = cut (\(_, min') -> min' >= n)
