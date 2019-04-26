-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0

-- 🇬🇧
-- Greatest common divisor algorithm
-- 🇧🇷
-- Máximo divisor comum

-- @param a (integer)
-- @param b (integer)
-- @return (integer) d = gcd(a,b)

myGCD :: Integral t => t -> t -> t
myGCD 0 0 = error "'a' or 'b' must be different from zero!"
myGCD 0 a = abs a
myGCD a 0 = abs a
myGCD a b = myGCD smaller remainder
  where
    smaller = min (abs a) (abs b)
    larger = max (abs a) (abs b)
    remainder = mod larger smaller
              
