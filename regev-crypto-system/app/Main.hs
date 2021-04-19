module Main where

import Decrypt (decryptBit)
import Encrypt (encryptBit, encryptedSize, getSubSet)
import Keys (genPrivateKey, genPublicKey, privateKeySize, publicKeySize)
import Lib (calcM)
import Phi
import System.Random
import Zp

main :: IO ()
main = do
  let p = 1973
      n = 80
      message = [0, 0, 1, 0, 1, 0, 0, 1]
  m <- calcM p n
  privateKey <- genPrivateKey p n
  publicKey <- genPublicKey p n m privateKey
  encryptedMsg <- mapM (encryptBit p m publicKey) message
  let decryptedMsg = map (decryptBit p privateKey) encryptedMsg
      privSize = privateKeySize privateKey
      publSize = publicKeySize publicKey
      bitSize = encryptedSize . head $ encryptedMsg
      messageSize = bitSize * bitSize
  putStrLn "\nthe encrypted msg is"
  print encryptedMsg
  putStrLn $ "\n\nFor a encryption schema with security of " ++ show n ++ " bits and p = " ++ show p ++ ", we have:"
  putStrLn $ "\nthe privateKey size is " ++ show privSize ++ " * (Int size)"
  putStrLn $ "\nthe publicKey size is " ++ show publSize ++ " * (Int size)"
  putStrLn $ "\nthe encrypted bit size is " ++ show bitSize ++ " * (Int size)"
  putStrLn $ "\nthe message size is " ++ show messageSize ++ " * (Int size)"
  putStrLn "\nthe decrypted msg is"
  print decryptedMsg
