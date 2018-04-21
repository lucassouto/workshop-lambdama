module Main where

data DiaSemana  = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
                  deriving Show

main :: IO ()
main = do
  putStrLn "hello world"
  print Segunda