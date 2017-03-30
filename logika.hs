suma :: Integer -> Integer
suma n = if n == 0
            then 0
            else n + suma (n-1)

silnia :: Integer -> Integer
silnia n = if n == 0
              then 1
              else n * silnia (n-1)

fib :: Integer -> Integer
fib x = if x < 2
           then 1
           else fib (x-1) + fib (x-2)

nwd :: (Integer,Integer) -> Integer
nwd (x,y) = if y == 0
               then x
               else nwd(y,(x`mod`y))

maks :: [Integer] -> Integer
maks [] = 0
maks (x:xs)
  | x > maks xs = x
  | otherwise = maks xs

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Eq,Ord,Show,Read)

atree = Node 5 (Node 3 (Node 2 Empty Empty) (Node 4 Empty Empty)) (Node 7 (Node 6 Empty Empty) (Node 9 Empty (Node 23 Empty Empty)))

sumTree :: Num a => Tree a -> a
sumTree Empty = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

depth :: Tree a -> Integer
depth Empty = 0
depth (Node n t1 t2) = 1 + (max (depth t1) (depth t2))

binSearch ::(Integral a)=>Tree a -> a -> Bool
binSearch Empty _ = False
binSearch (Node x t1 t2) y
                      | x == y = True
                      | x > y = binSearch t1 y
                      | x < y = binSearch t2 y

preorder :: (Integral a) => Tree a -> [a]
preorder Empty = []
preorder (Node x t1 t2) = [x] ++ preorder t1 ++ preorder t2

listadd :: [Integer] -> [Integer] -> [Integer]
listadd [] xs = xs
listadd xs [] = xs
listadd (x:xs) (y:ys) = [x+y] ++ (listadd xs ys)

insertt :: Integer -> [Integer] -> [Integer]
insertt x xs = lesser xs ++ [x] ++ greater xs
  where
    lesser xs = [y | y <-xs , y<=x]
    greater xs = [y | y <-xs , y>x]

insort :: [Integer] -> [Integer]
insort [] = []
insort xs = insertt y (insort ys)
  where
    y = head xs
    ys = tail xs

compose :: (a->b)->(b->c)->(a->c)
compose f g = (\x -> g (f x))

curry :: ((a,b)->c)->(a->b->c)
curry f a b = f (a,b)

uncurry :: (a->b->c)->((a,b)->c)
uncurry f (a,b) = f a b

multifun :: (a->a)->Integer->(a->a)
multifun f 1 = (\x -> f x)
multifun f n = compose f (multifun f (n-1))

ltake :: [a] -> Integer -> [a]
ltake xs 0 = []
ltake [] i = []
ltake (x:xs) i = [x] ++ (ltake xs (i-1))

--lall :: (a->Bool)->[a]->Bool
lall f (x:xs) = if f head
                   then lall f (tail xs)
                   else False

lmap :: (a->a)->[a]->[a]
lmap f xs = [f x | x<-xs]

lrev :: [a]->[a]
lrev [] = []
lrev xs = [last xs] ++ (lrev (init xs))

lzip :: [a] -> [b] -> [(a,b)]
lzip xs [] = []
lzip [] ys = []
lzip (x:xs) (y:ys) = [(x,y)] ++ (lzip xs ys)

split :: [a] -> ([a],[a])
split (x:[]) = ([x],[])
split [] = ([],[])
split (x:y:xs) = ([x]++t1,[y]++t2)
  where
    (t1,t2) = split xs

cartprod :: [a] -> [a] -> [(a,a)]
cartprod [] ys = []
cartprod xs [] = []
cartprod xs ys = [(x,y) | x<-xs, y<-ys]
