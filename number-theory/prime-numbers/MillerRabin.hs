-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:
-- Miller-Rabin's algorithm to test primality
-- This is a probabilistic algorithm
-- False means it is NOT a prime
-- True means it probably is prime
-- First parameter is a integer to be tested
-- The second is how many times it must be tested

-- 🇧🇷
-- Algoritmo de Miller-Rabin para testar primalidade
-- É um algoritmo probabilístico
-- False significa que o número NÃO é primo
-- True significa que o número provavelmente é primo
-- O primeiro parâmetro é um inteiro a ser testado
-- O segundo é quantas vezes ele deve ser testado

-- miller_rabin_test
-- @param prime (integer)
-- @param repeat (integer)
-- @return is_prime (Bool)

module MillerRabin where
import System.Random
import ModularExp

test_pure :: (Integral t, Random t) => t -> [t] -> Bool
test_pure prime arr = is_prime
  where
    booleans = map (\a -> unitary_test prime a) arr
    is_prime = foldl (&&) True booleans

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
    exps = foldl (\acc r -> (mod_exp (head acc) 2 prime):acc ) [first_exp] [1..(r-1)]
    minus_one = mod (-1) prime
    congruent_to_one = first_exp == 1 || first_exp == -1
    any_congruent_to_minus_one = foldl (\acc x -> acc || (mod x prime)==minus_one) False exps
    result = congruent_to_one || any_congruent_to_minus_one

decompose :: (Integral a) => a -> (a,a)
decompose a = decompose_aux (a-1) 0

decompose_aux :: (Integral a) => a -> a -> (a,a)
decompose_aux a r
  | mod a 2 == 1 = (a,r)
  | otherwise = decompose_aux (div a 2) (r+1)
