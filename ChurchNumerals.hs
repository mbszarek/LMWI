-- Mateusz Szarek
-- Computer Science Student
-- Faculty of Computer Science, Electronics and Telecommunication
-- AGH University of Science and Technology
-- 2016/2017 Academic Year

{-# LANGUAGE RankNTypes #-}

newtype Church = Ch ( forall a . (a->a)->(a->a))

zero :: Church
zero = Ch (\f x -> x)
one :: Church
one = Ch (\f x -> f x)
two :: Church
two = Ch (\f x -> f (f x))
three :: Church
three = Ch (\f x -> f (f (f x)))
four :: Church
four = Ch (\f x -> f (f (f (f x))))
five :: Church
five = Ch (\f x -> f (f (f (f (f x)))))
six :: Church
six = Ch (\f x -> f (f (f (f (f (f x))))))
seven :: Church
seven = Ch (\f x -> f (f (f (f (f (f (f x)))))))
eight :: Church
eight = Ch (\f x -> f (f (f (f (f (f (f (f x))))))))
nine :: Church
nine = Ch (\f x -> f (f (f (f (f (f (f (f (f x)))))))))
ten :: Church
ten = Ch (\f x -> f (f (f (f (f (f (f (f (f (f x))))))))))

true = (\x y -> x)

false = (\x y -> y)

_if = (\b x y -> b x y)

to_boolean x = _if x True False

iszero (Ch n) = n (\x -> false) true
isequal n m = _and (iszero (_subtract n m)) (iszero (_subtract m n))
isleeq n m = iszero (_subtract n m)
isle n m = _and (isleeq n m) (_not (isequal n m))
isgreq n m = iszero (_subtract m n)
isgr n m = _and (isle m n) (_not (isequal m n))

increment :: Church -> Church
increment (Ch n) = Ch (\p x -> p (n p x))

decrement :: Church -> Church
decrement (Ch n) = Ch (\f x -> n (\g h -> h (g f)) (\y -> x) (\y -> y))

add :: Church -> Church -> Church
add (Ch m) (Ch n) = Ch (\f x -> m f ((n f) x))

_subtract :: Church -> Church -> Church
_subtract m (Ch n) = n decrement m

multiply :: Church -> Church -> Church
multiply (Ch m) (Ch n) = Ch (\x -> m (n x))

power :: Church -> Church -> Church
power (Ch m) (Ch n) = Ch (n m)

_mod m n = isgreq m n (_mod (_subtract m n) n) m

_not = (\x -> x false true)
_and = (\x y -> x y false)
_or = (\x y -> x true y)
_xor = (\x y -> x (_not y) y)

pair x y = (\f -> f x y)
first p = p (\x y -> x)
second p = p (\x y -> y)

_empty = pair true true
_isempty = first
_unshift = (\l x -> pair false (pair x l))
_first = (\l -> first (second l))
_rest = (\l -> second (second l))

ycomb :: (a -> a) -> a
ycomb f = f (ycomb f)

_sqrt = ycomb (\f a b -> (isgr (power a two) b) (decrement a) $ f (increment a) b) one
fib = ycomb $ \f n -> (isleeq n one) one $ add (f $ _subtract n two) (f $ decrement n)
isprime = ycomb (\f a b -> (iszero $ _mod b a) false $ (isle a $ _sqrt b) (f (increment a) b) true) two
_gcd = ycomb (\f a b -> (isequal a b) a $ (isle a b) (f a $ _subtract b a) $ f (_subtract a b) b)


to_integer::Church->Integer
to_integer (Ch n) = n (\x->x+1) 0

my_list = _unshift (_unshift (_unshift _empty three) two) one

main = do putStrLn "Zero:"
          print (to_integer zero)
          putStrLn "One:"
          print (to_integer one)
          putStrLn "Five++:"
          print (to_integer (increment five))
          putStrLn "Ten + Nine--:"
          print (to_integer (add ten (decrement nine)))
          putStrLn "Six modulo Seven:"
          print (to_integer (_mod six seven))
          putStrLn "Is seven lower than six?:"
          print (to_boolean (isle seven six))
          putStrLn "And what about seven and nine?:"
          print (to_boolean (isle seven nine))
          putStrLn "Five to the power of 4:"
          print (to_integer (power five four))
          putStrLn "10 times 9:"
          print (to_integer (multiply ten nine))
          putStrLn "7 to the power of 3 minus 21:"
          print (to_integer (_subtract (power seven three) (multiply seven three)))
          putStrLn "20th Fibonacci's number:"
          print (to_integer (fib (multiply ten two)))
          putStrLn "Is 47 prime?:"
          print (to_boolean (isprime (add (multiply ten four) seven)))
          putStrLn "And 49?:"
          print (to_boolean (isprime (_subtract (multiply ten five) one)))
          putStrLn "What is the greatest common divider of 35 and 63?:"
          print (to_integer (_gcd (multiply seven five) (multiply seven nine)))
          putStrLn "What is the square root of 1024?:"
          print (to_integer (_sqrt (add (multiply (multiply ten ten) ten) (add (multiply ten two) four))))
          putStrLn "Second element from my_list:"
          print (to_integer (_first (_rest my_list)))
