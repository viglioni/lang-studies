-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- 🇬🇧
-- N-th fibonacci sequence algorith
-- ATENTION: THIS ALG RUNS IN TIME EXPONENTIAL. DO NOT USE FOR LARGE VALUES OF N!!!
-- 🇧🇷
-- Algoritmo para retornar o n-ésimo termo da seqüência de fibonacci
-- ATENÇÃO: ESTE ALGORITMO RODA EM TEMPO EXPONENCIAL. NÃO USE PARA VALORES ALTOS DE N!!!

-- @param  n (non-negative integer)
-- @return f (non-netative integer)
fibonacci :: (Integral n) => n -> n
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n
  | n < 0 = error "n must be non-negative!"
  | otherwise = (fibonacci (n-1)) + (fibonacci (n-2))


