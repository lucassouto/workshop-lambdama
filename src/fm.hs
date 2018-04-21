{-- Functors --}

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

--

map :: (a->b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

inc = map (+1)
sqr = map (^2)

{-- 
De forma mais geral, a idea de mapear uma função para cada
elemento de uma estrutura de dados não é limitada apenas para listas,
mas para qualquer tipo de dados parametrizados. Em Haskell a classe 
de tipos que suporta essa operação é chamada de Functor.
--}

class Functor f where
  fmap :: (a->b) -> f a -> f b

{--
Ou seja, um functor aplica uma função (a->b) em uma estrura f a, 
que contem elementos do tipo a e retorna uma estrutura f b, com 
elementos do tipo b
--}

fmap (+1) Nohting
fmap (+1) (Just 3)

{--
Tipos definidos pelo usuario também podem se tornar functores. Por
Exemplo:
--}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a->b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

{--
Leis dos Functores:
  - fmap id = id
  - fmap (g . h) = fmap g . fmap h
--}

{-- Aplicatives --}

{--
Mas e se quisermos aplicar funções com uma quantidade arbitrária
de argumentos?
--}

fmap0 :: a -> f a
fmap1 :: (a->b) -> f a -> f b
fmap2 :: (a->b->c) -> f a -> f b -> f c
fmap3 :: (a->b->c->d) -> f a -> f b -> f c -> f d
(...)

{-- 
Podemos generalizar isso usando a idea de Currying, mais essas 
duas funções
--}

pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b

{--
pure converte um valor do tipo `a` para uma estrutura do tipo `f a`
e o <*>...
--}

Prelude> pure (+1) <*> [1,2,3]
[2,3,4]
Prelude> pure (+) <*> [1] <*> [1,2,3]
[2,3,4]
Prelude> pure (+) <*> [1,2] <*> [1,2,3]
[2,3,4,3,4,5]
Prelude> pure (*) <*> [1,2] <*> [1,2,3]
[1,2,3,2,4,6]
Prelude> pure (+2) <*> Just 3
Just 5
Prelude> pure (+2) <*> Nothing
Nothing

{-- Leis das Aplicatives
pure id <*> x = x
pure (g x) = pure g <*> pure x
x <*> pure y = pure (\g -> g y) <*> x
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
--}

{-- Monads --}