-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0

-- 🇬
-- Extended greatest common divisor algorithm
-- Given two integer numbers, a and b, a<b, returns d=gcd(a,b) and a pair (s,t) such that d = s*a + t*b
-- if a < b, it will return (d,s,t) such that d = s*b + t*a

-- 🇧🇷
-- Máximo divisor comum - extendido
-- Dados dois números inteiros, a e b, a<b, retorna d=mdc(a,b) e um par (s,t) tal que such that d = s*a + t*b
-- Se a < b, retornará (d,s,t) tal que d = s*b + t*a

-- @param a (integer)
-- @param b (integer)
-- @return d,s,t (integer, integer, integer)

eGCD :: Integral t => t -> t -> (t, t, t)
eGCD 0 0 = error "'a' or 'b' must be different from zero!"
eGCD a b = recursiveEGCD (abs a) (abs b) 1 0 0 1 quotient
  where
    quotient = div larger smaller
    larger = max (abs a) (abs b)
    smaller = min (abs a) (abs b)


-- The main method (eGCD) uses the algorithm bellow with the initial params:
-- O método principal (eGCD) usa o algoritmo abaixo com os parâmetros iniciais:
-- s0 = 1, t0 = 0, s1 = 0, t1 = 1, q = int(a/b)
recursiveEGCD :: Integral t => t -> t -> t -> t -> t -> t -> t -> (t, t, t)
recursiveEGCD a 0 s0 t0 _ _ _ = (abs a, s0, t0)
recursiveEGCD 0 a _ _ s1 t1 _ = (abs a, s1, t1)
recursiveEGCD a b s0 t0 s1 t1 q = recursiveEGCD smaller remainder s1 t1 s2 t2 quotient
  where
    smaller = min (abs a) (abs b)
    larger = max (abs a) (abs b)
    remainder = mod larger smaller
    quotient = div smaller remainder
    s2 = s0 - q*s1
    t2 = t0 - q*t1
