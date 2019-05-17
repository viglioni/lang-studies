-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:
-- Modular exponentiation: exponentiation by square
-- Fast way to computate mod (a^b) c, where a,b and c are big numbers

-- 🇧🇷
-- Exponenciação modular: exponenciação por quadrados

-- @param base (integer)
-- @param exponent (integer)
-- @param modulus (integer)
-- @return result (integer)

module ModularExp (mod_exp) where


mod_exp :: (Integral t) => t -> t-> t -> t
mod_exp base exponent modulus = mod_exp_aux base exponent modulus 1

mod_exp_aux :: (Integral t) => t -> t -> t -> t -> t
mod_exp_aux base 0 modulus result = result
mod_exp_aux base exponent modulus result
  |mod exponent 2 == 1 = mod_exp_aux new_base new_exponent modulus new_result
  |otherwise = mod_exp_aux new_base new_exponent modulus result
  where
    new_exponent = div exponent 2
    new_result = mod (base*result) modulus
    new_base = mod (base*base) modulus

