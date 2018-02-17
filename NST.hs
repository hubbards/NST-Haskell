-- | This module contains an implementation of the nested sequence of tuples
--   (NST) algebra from the paper "An Algebra for Structured Office Documents"
--   by Ralf Hartmut Güting, Roberito Zicari, and David M. Choy.
--
--   For simplicity, only simple operations are implemented.
--
module NST where

import qualified Data.List as L
import qualified Control.Monad as M

-- -----------------------------------------------------------------------------
-- NST model

-- | A tuple is a non-empty list.
data Tuple a = T { thead :: a, ttail :: [a] }

-- | Transform tuple into non-empty list.
toList :: Tuple a -> [a]
toList t = thead t : ttail t

-- | Transform non-empty list into tuple.
toTuple :: [a] -> Tuple a
toTuple xs = T { thead = head xs, ttail = tail xs }

instance Show a => Show (Tuple a) where
  show t = "(" ++ show (thead t) ++ helper (ttail t) ++ ")" where
    helper []       = ""
    helper (x : xs) = ", " ++ show x ++ helper xs

instance Eq a => Eq (Tuple a) where
  x == y = thead x == thead y && ttail x == ttail y

-- | Atomic value (or instance).
data Atom = I Int    -- integer value
          | B Bool   -- Boolean value
          | S String -- string value
  deriving Eq

-- | Higher-order helper function for atomic values.
atom :: (Int -> a) -> (Bool -> a) -> (String -> a) -> Atom -> a
atom f _ _ (I i) = f i
atom _ g _ (B b) = g b
atom _ _ h (S s) = h s

-- | Show instance for atomic values.
instance Show Atom where
  show = atom show show show

-- | Composit value (or instance).
data Comp = A Atom         -- atom
          | L [Tuple Comp] -- sequence (or list) of tuples
  deriving Eq

-- | Higher-order helper function for composit values.
comp :: (Atom -> a) -> ([Tuple Comp] -> a) -> Comp -> a
comp f _ (A v)  = f v
comp f g (L ts) = g ts

-- | Show instance for composit values.
instance Show Comp where
  show = comp show show

-- | Return composit value of all zeros with given sequence and tuple lengths.
--   Sequence length must be non-negative and tuple length must be positive.
zeros :: Int -> Int -> Comp
zeros i j
  | i >= 0 && j > 0 = L . (replicate i) . toTuple . (replicate j) $ A (I 0)
  | otherwise       = error $ "illegal lengths: " ++ show i ++ ", " ++ show j

-- -----------------------------------------------------------------------------
-- Check schema

-- | Check if two atomic values have the same schema.
acheck :: Atom -> Atom -> Bool
acheck (I _) = atom (const True) (const False) (const False)
acheck (B _) = atom (const False) (const True) (const False)
acheck (S _) = atom (const False) (const False) (const True)

-- | Checks if two tuples have the same schema.
tcheck :: Tuple Comp -> Tuple Comp -> Bool
tcheck x y = ccheck (thead x) (thead y) && helper (ttail x) (ttail y) where
  helper [] []                     = True
  helper xxs@(x : xs) yys@(y : ys) = tcheck (toTuple xxs) (toTuple yys)
  helper _ _                       = False

-- | Checks if two composit values have the same schema.
ccheck :: Comp -> Comp -> Bool
ccheck (A x)        = comp (\y -> acheck x y) (const False)
ccheck (L [])       = comp (const False) (const True)
ccheck (L (x : xs)) = comp (const False) helper where
  helper []       = True
  helper (y : ys) = tcheck x y && ccheck (L xs) (L ys)

-- | Checks if each tuple in a sequence has the same schema.
scheck :: Comp -> Bool
scheck = comp lfail helper where
  helper []       = True
  helper (x : xs) = ccheck (L [x]) (L xs)

-- | Error message when composit value is not a sequence
lfail :: Atom -> a
lfail x = error $ "atomic value: " ++ show x

-- -----------------------------------------------------------------------------
-- NST algebra

type Op1 = [Tuple Comp] -> [Tuple Comp]
type Op2 = [Tuple Comp] -> Op1

-- | Highter-order helper function for unary operation on sequences.
comp1 :: Op1 -> Comp -> Comp
comp1 op = L . comp lfail op

-- | Highter-order helper function for binary operation on sequences.
comp2 :: Op2 -> Comp -> Comp -> Comp
comp2 op (L xs) (L ys) = L (xs `op` ys)
comp2 _ _ _ = error "composit values must be sequences"

-- | Tuple concatenation.
(<->) :: Tuple a -> Tuple a -> Tuple a
x <-> y = toTuple (toList x ++ toList y)

-- | Sequence concatenation.
(<+>) :: Comp -> Comp -> Comp
(<+>) = comp2 (++)

-- | Head of sequence.
chead :: Comp -> Comp
chead = comp1 (return . head)

-- | Tail of sequence.
ctail :: Comp -> Comp
ctail = comp1 tail

-- | Removes duplicates.
rdup :: Comp -> Comp
rdup = comp1 L.nub

-- | Projection. The list of indices must not be empty.
proj :: [Int] -> Comp -> Comp
proj is = comp1 $ map (toTuple . (helper ds) . toList) where
  ds = let js = L.nub (L.sort is) in head js : zipWith (-) (tail js) js
  helper (k : ks) xs = let ys = drop k xs in head ys : helper ks (tail ys)
  helper [] _        = []

-- | Cartesian product.
prod :: Comp -> Comp -> Comp
prod = comp2 $ \xs ys -> (<->) <$> xs <*> ys

-- | Pair.
pair :: Comp -> Comp -> Comp
pair = comp2 $ zipWith (<->)

-- | Union.
union :: Comp -> Comp -> Comp
x `union` y = rdup (x <+> y)

-- | Intersection.
inter :: Comp -> Comp -> Comp
inter = comp2 L.intersect

-- | Difference.
diff :: Comp -> Comp -> Comp
diff = comp2 (L.\\)
