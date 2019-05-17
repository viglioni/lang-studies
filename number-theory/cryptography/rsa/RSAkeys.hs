-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- 🇬🇧
-- Get public key (e,n) and a corresponding private key (d) using two primes p and q
-- One must install System.Random
-- $ cabal install random
-- Extendend GCD and GCD algorithm is also needed

-- 🇧🇷
-- Cria a chave pública (n,e) e a chave privada (d) correspondente, a partir de dois primos p e q
-- É necessário instalar System.Random
-- $ cabal install random
-- Os algoritmos de MDC e MDC expandidos são necessários

-- generate_keys
-- @param  p (prime number)
-- @param  q (prime number)
-- @return (public_key, private_key) (non-netative integer)

module RSAkeys (generate_keys) where

import System.Random
import EGcd
import PrimeGenerator


generate_keys :: (Integral t, Random t) => t -> t -> IO ((t,t),t) 
generate_keys p q =
  do
    seed <- newStdGen
    return $ gen_keys_pure p q seed

generate_keys_by_bits :: (Integral a, Random a) => a -> IO ((a,a),a)
generate_keys_by_bits bits =
  do
    seed <- newStdGen
    return $ gen_keys_by_bits_pure bits seed

gen_keys_by_bits_pure :: (Integral a, Random a, RandomGen g) => a -> g -> ((a,a),a)
gen_keys_by_bits_pure bits seed = gen_keys_pure p q seed
  where
    min = (2^bits)
    max = (2^(bits+1))
    seed1 = snd $ next seed
    seed2 = snd $ next seed1
    p = PrimeGenerator.prime_gen_pure min max seed1
    q = PrimeGenerator.prime_gen_pure min max seed2
    
  
    

gen_keys_pure :: (Integral t, Random t, RandomGen a) => t -> t -> a -> ((t,t),t)
gen_keys_pure p q seed = (public, private)
  where
    n = p*q
    phi = (p-1)*(q-1)
    e = get_public phi seed 0
    public = (e,n)
    private = get_private phi e


get_public :: (Integral n, Random n, RandomGen g) => n -> g -> n -> n
get_public phi seed counter
  | gcd e phi == 1 = e
  | otherwise = get_public phi seed new_counter
  where
    e = (fst $ randomR (1, ((phi)-1) ) seed) + counter
    new_counter = mod (counter+1) phi
           
get_private :: (Integral a) => a -> a -> a
get_private phi e = mod coef_from_egcd phi
  where coef_from_egcd = trd $ eGCD phi e


trd :: (a,b,c) -> c
trd (_,_,x) = x



