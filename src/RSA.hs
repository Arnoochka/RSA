module RSA (
        generateKeys,
        encrypt,
        decrypt,
        example
        ) where

import Lib
import Data.Char

-- реализация RSA

-- алгоритм генерации открытого и закрытого ключа   
generateKeys :: Int -> Int -> (Int, Int, Int)
generateKeys p q = 
    let
        n = p * q
        modulus = (p - 1) * (q - 1)
        e = findCoprime modulus
        (_, d, _) = extendedGCD e modulus

    in 
    if d < 0 then (e, d + modulus, n)
    else (e, d, n)

-- Штфрование сообщения
encrypt :: (Int, Int) -> String -> [Int]
encrypt (e, n) message = 
    let 
        toInt x = modExp (ord x) e n 
    in map toInt message

-- дешифрование сообщения
decrypt :: (Int, Int) -> [Int] -> String
decrypt (d, n) message = 
    let 
        toStr = \x -> chr (modExp x d n)
    in map toStr message

example :: IO ()
example = do
    message <- getLine
    let
        p = 1019
        q = 1031
        (e, d, n) = generateKeys p q
        encryptMessage = encrypt (e, n) message
        decryptMessage = decrypt (d, n) encryptMessage

    print $ "Public key: " ++ show (e, n) ++ " " ++ "Private key: " ++ show (d, n)
    print $ "encrypt: " ++ show encryptMessage
    print $ "decrypt: " ++ decryptMessage