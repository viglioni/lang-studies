-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:

-- 🇧🇷

-- prime_generator
-- @param a (integer)
-- @param b (integer)
-- @return p (integer)

-- prime_gen_by_bits
-- @param a (integer)
-- @return p (integer)

module MillerRabin where
import System.Random
import ModularExp

miller_rabin_test :: (Integral t, Random t) => t -> t -> IO Bool
miller_rabin_test prime repeat =
  do
    seed <- newStdGen
    return $ mr_test prime repeat seed 1 True

mr_test :: (Integral t, RandomGen g, Random t) => t -> t -> g -> t -> Bool -> Bool
mr_test 2 _ _ _ _ = True
mr_test prime repeat seed counter acc =
  if prime < 2 then False
  else
    if counter <= repeat && acc
    then mr_test prime repeat new_seed (counter+1) test
    else acc
  where
    (base, new_seed) = randomR (2, (prime-1)) seed
    test = acc && unitary_test prime base

unitary_test prime base = if (mod prime 2) == 0 then False else result
  where
    (a,r) = decompose prime
    first_exp = mod_exp base a prime
    exps = map (\r -> mod_exp first_exp r prime) [1..r]
    minus_one = mod (-1) prime
    congruent_to_one = (mod first_exp prime == 1)
    any_congruent_to_minus_one = foldl (\acc x -> acc || (mod x prime)==minus_one) False exps
    result = congruent_to_one || any_congruent_to_minus_one

decompose :: (Integral a) => a -> (a,a)
decompose a = decompose_aux (a-1) 0

decompose_aux :: (Integral a) => a -> a -> (a,a)
decompose_aux a r
  | mod a 2 == 1 = (a,r)
  | otherwise = decompose_aux (div a 2) (r+1)
