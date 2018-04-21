module Main where

data DiaSemana  = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
                  deriving Eq

proximoDia :: DiaSemana -> DiaSemana

proximoDia Segunda = Terca
proximoDia Terca = Quarta
proximoDia Quarta = Quinta
proximoDia Quinta = Sexta
proximoDia Sexta = Sabado
proximoDia Sabado = Domingo
proximoDia Domingo = Segunda

data Booleano = V | F 
                deriving (Show, Eq)

instance Show DiaSemana where
  show Segunda = "S"
  show Terca = "T"
  show Quarta = "Q"
  show Quinta = "QT"

(&&@) :: Booleano -> Booleano -> Booleano

V &&@ x = x
F &&@ _ = F

proximoDiaLista :: [DiaSemana] -> [DiaSemana]
proximoDiaLista [] = []
proximoDiaLista (x:xs) = proximoDia x : proximoDiaLista xs

todos' :: [Bool] -> Bool
todos' [] = True
todos' (x:xs) = x && todos' xs

soma' :: [Int] -> Int
soma' [] = 0
soma' (x:xs) = x + soma' xs

todos'' :: [Bool] -> Bool
todos'' = foldr (&&) True

main :: IO ()
main = do
  putStrLn "hello world"
  print $ proximoDia Segunda
  print $ proximoDiaLista [Segunda]
  let x = [Segunda, Terca, Domingo]
  print $ map  (proximoDia) x
  print $ todos' [True, False, True, True]
  print $ todos'' [True, False]
