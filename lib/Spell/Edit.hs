{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Spell.Edit
-- Description : Calculation of Minimum Edit Distance (MED).
-- Copyright   : (c) Stefan Haller, 2014
--
-- License     : MIT
-- Maintainer  : s6171690@mail.zih.tu-dresden.de
-- Stability   : experimental
-- Portability : portable
--
-- This module allows to calculate the
-- <http://en.wikipedia.org/wiki/Minimum_Edit_Distance Minimum Edit Distance>
-- using <http://en.wikipedia.org/wiki/Trie prefix trees>.
module Spell.Edit where

import           Control.Monad (guard)

import           Data.Char (toLower)
import           Data.ListLike.Instances ()
import           Data.Maybe (listToMaybe, maybeToList)
import qualified Data.PQueue.Prio.Min as PMin
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Trie (Trie, cut, branches, end, expandPaths, populate, update, value)
import           Data.Tuple (swap)
import           Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V


-- | Aggregation of penalty functions.
--
-- All these functions receive both characters as arguments. The first argument
-- for the insertion and deletion is optional (because insertions and deletions
-- at the beginning of the word do not have a reference character).
--
-- Note that the function calculate the penalties for an edit operation where
-- the expected character is modified to match the character of the input.
--
-- (This behavior is the same as in the reference literature: Mark D. Kernighan,
-- Kenneth W. Church und William A. Gale. „A spelling correction program based
-- on a noisy channel model“)
data Penalties a p = Penalties
    { penaltyInsertion    :: Maybe a -> a -> p
    , penaltyDeletion     :: Maybe a -> a -> p
    , penaltySubstitution ::       a -> a -> p
    , penaltyReversal     ::       a -> a -> p
    }


-- | Penalty functions where all edit operations have a cost of @1@.
defaultPenalties :: Penalties Char Double
defaultPenalties = Penalties
    { penaltyInsertion    = \_ _ -> 1
    , penaltyDeletion     = \_ _ -> 1
    , penaltyReversal     = \_ _ -> 1
    , penaltySubstitution = subst
    }
  where
    subst x y
      | x == y                 = 0.0
      | toLower x == toLower y = 0.5
      | otherwise              = 1.0

-- | Calculates the column vectors for the nodes in the 'Trie'. Meant
-- to be used with 'populate'.
calculateEdit :: (Num p, Ord p, Unbox p)
                 => Penalties Char p -> Text -> [Char] -> [Vector p] -> Vector p
calculateEdit p r = f
  where
    -- Builds the column vector.
    --
    -- The first column vector (pattern match on empty path) contains the
    -- numbers [0, 1, 2, ...] in the paper. In general this is: [0,
    -- penaltyInsert_1, penaltyInsert_1 + penaltyInsert_2, ...].
    f [] [] = V.scanl (+) 0 . V.map (penaltyInsertion p Nothing) . V.fromList $ T.unpack r
    -- For all other column vectors we construct a vector of the right size and
    -- use a function produce all values.
    --
    -- constructN will build a vector of size n and apply our function with a
    -- vector of growing size until all elements are set. So on our first call
    -- the vector has size 0, then size 1, etc.
    f ks vs = V.constructN (T.length r + 1) (f' ks vs)

    -- This function will produce the value for the next row of our column
    -- vector. We start with the lowest cell. See the ks and vs as a stack where
    -- the ks represents the current path in our Trie and the vs all the values
    -- on the path.
    --
    -- v' is the growing vector we are going to fill.
    f' (k:ks) (v:vs) v'
      | V.null v' = v V.! i + penaltyDeletion p (listToMaybe ks) k
      | otherwise = minimum choices
      where
        i = V.length v'

        choices = [ v  V.!   i   + penaltyDeletion p (listToMaybe ks) k
                  , v' V.! (i-1) + penaltyInsertion p (Just k) (r `T.index` (i-1))
                  , v  V.! (i-1) + penaltySubstitution p k (r `T.index` (i-1))
                  ] ++ maybeToList reversal

        -- This is like (T.!), but returns Nothing if out of bounds.
        (!?) :: Text -> Int -> Maybe Char
        (!?) t n
          | n < 0     = Nothing
          | otherwise = fmap fst . T.uncons $ T.drop n t

        -- Checks for reversal. Returns the costs for the reversal or Nothing if
        -- no reversal is possible.
        reversal = do
          let s = [Just k, listToMaybe ks]
              t = [r !? (i-2), r !? (i-1)]
          [s1, s2] <- sequence s -- guard against Nothings
          guard (s == t)         -- check that it’s really a reversal
          v2 <- listToMaybe vs   -- pop the second element from vector stack
          return $ v2 V.! (i-2) + penaltyReversal p s2 s1

    f' _ _ _ = error "This function should never get called."

-- | Shrinks the column vectors of the 'Trie' nodes.
--
-- The calculation of the best edit does not need the whole vectors. It
-- only needs:
--
--  * The last (top-most) element of the vector, because this represents
--    the edit distance of the current node.
--
--  * The minimum element of the vector, because we later use this value as
--    heuristic. Each edit path must pass this column vector and the scores
--    can only get higher. If we have an edit with a value smaller than
--    this minimum, we can completely ignore this node.
--
-- Due to reversals the result might get wrong if we only consider the minimum
-- of the current vector. Reversals allow jumps and the minimum element of the
-- vector might decrease. To fix this we take the minimum of the current and the
-- previous vector (the reversal costs only skip one column vector).
shrinkMatrices :: (Ord p, Unbox p) => Trie Char (Vector p) -> Trie Char (p, p)
shrinkMatrices = update f
  where
    f []        _ _ = undefined
    f [v1]      _ _ = (V.last v1, V.minimum v1)
    f (v1:v2:_) _ _ = (V.last v1, min (V.minimum v1) (V.minimum v2))

-- | Lazily returns all suggestions sorted in order of increasing edit distance.
--
-- To calculate only the best suggestion use @head . searchBestEdits@. To
-- get the best 10 suggestions you can use @take 10 . searchBestEdit@.
--
-- The internal implementation uses some kind of greedy algorithm (similiar
-- to Dijkstra or A*, but to simplified for a tree structure):
--
-- There are two priority queues:
--
--  * The queue for end nodes (“finished queue”). The priority measure is
--    the minimum edit distance (= the top-most element of the column
--    vector).
--
--  * The queue for discovered nodes (“working queue”). The priority measure is the
--    minimum value of the colum vector.
--
-- After initializing the working queue with the given 'Trie' node, the
-- algorithm works as follows:
--
--  * Take the first element of working queue.
--
--  * If the value is greater than the minimum value in finished queue, the
--    next finished node is optimal. All paths must pass through exactly
--    one cell of each column vector of the path. The current working node
--    is on the best undiscoverd path. If the value of the next finished
--    node is lower, it must be optimal. So we return the next finished
--    node as the next result.
--
--  * Expand children of current working node and insert them into the
--    working queue.
--
--  * If the current working node is an end node, insert it into the
--    finished queue.
--
--  * In any case, the current working node is dropped from the working
--    queue.
--
--  * Repeat everything until the working queue is empty.
searchBestEdits :: (Num p, Ord p) => Trie Char (Text, (p, p)) -> [(Text, p)]
searchBestEdits trie = map swap $ processQueue (finished, queue)
  where
    finished = PMin.empty
    queue    = PMin.singleton 0 trie

    -- This function iterates over working queue till empty.
    processQueue (f, q)
      | PMin.null q                    = PMin.toAscList f                  -- working queue empty
      | not (PMin.null f) && f'' < q'' = h : processQueue (f', q)          -- element is optimal
      | otherwise                      = processQueue $ processNext (f, q) -- not sure enough, recursion
      where
        Just (h@(f'', _), f') = PMin.minViewWithKey f
        (q'', _) = PMin.findMin q

    -- This function processes the head element of the working queue.  In every
    -- case the current head is expanded and the children are inserted into the
    -- working queue.
    processNext (f, q)
      | end t     = (f', q'') -- reinsert element in finished queue
      | otherwise = (f,  q'') -- keep finished queue,
      where
        Just (t, q') = PMin.minView q
        (path, (last', _)) = value t
        f' = PMin.insert last' path f
        q'' = q' `PMin.union` PMin.fromList (map (processBranches .snd) $ branches t)

    -- Calculates values for all the branches (children).
    processBranches t = let (_, (_, min')) = value t in (min', t)

-- | Like 'searchBestEdits' but only returns the resulting 'Text's.
searchBestEdits' :: (Num p, Ord p) => Trie Char (Text, (p, p)) -> [Text]
searchBestEdits' = map fst . searchBestEdits

-- | One-shot function for determining the best suggestions.
--
-- This function works lazily, for more information see 'searchBestEdits'.
bestEdits :: (Num p, Ord p, Unbox p)
             => Penalties Char p -- ^ The penalties used for calculating the MED.
             -> Maybe p          -- ^ Nodes where the minimum of the column
                                 --   vector is greater (or equal) than this
                                 --   'Just' value are cutted. 'Nothing'
                                 --   prevents cutting.
             -> Text             -- ^ The reference word.
             -> Trie Char ()     -- ^ The 'Trie' 'Data.Trie.skeleton'.
             -> [(Text, p)]
bestEdits p c r = searchBestEdits
                . expandPaths
                . maybe id doCut c
                . shrinkMatrices
                . populate (calculateEdit p r)
  where
    doCut n = cut (\(_, min') -> min' >= n)

-- | Like 'bestEdits' but only returns the resulting 'Text's.
bestEdits' :: (Num p, Ord p, Unbox p)
              => Penalties Char p -> Maybe p -> Text -> Trie Char () -> [Text]
bestEdits' p c r = map fst . bestEdits p c r
