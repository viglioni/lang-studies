-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- 🇬🇧
-- N-th fibonacci sequence algorith
-- Using dynamic programming
-- 🇧🇷
-- Algoritmo para retornar o n-ésimo termo da seqüência de fibonacci
-- Usando programação dinâmica

-- @param  n (non-negative integer)
-- @return f (non-netative integer)
fibonacci :: (Integral n) => n -> n
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n
  | n < 0 = error "n must be non-negative!"
  | otherwise = auxfib n 2 1 2


auxfib :: (Eq a, Integral a) => a -> a -> a -> a -> a  
auxfib n result prev counter =
  if counter == n
  then result
  else auxfib n (result + prev) result (counter + 1)
