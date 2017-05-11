--Problem 1
mylast :: [a] -> a
mylast []       = error "Lista vacia"
mylast [x]      = x
mylast (_:xs)   = mylast xs

mylast1 [x]     = Just x
mylast1 []      = Nothing
mylast1 (_:xs)  = mylast1 xs

mylast2 [x]     = Right x
mylast2 []      = Left "Lista vacia"
mylast2 (_:xs)  = mylast2 xs

--Problem 2
myButLast :: [a] -> a
myButLast [x]   = error "No hay suficientes elementos"
myButLast []    = error "Lista vacia"
myButLast xs    = if length xs == 2 then head xs else myButLast (tail xs)

--Problem 3
elementAt (e:_) 0    = e
elementAt [] _   = error "Indice fuera de rango"
elementAt (_:xs) n = if n<0 then error "Indice negativo" else elementAt xs (n-1)

--Problem 4
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse2 [] = []
myReverse2 (x:xs) = aux (x:xs) []
    where
        aux [] l = l 
        aux (x:xs) l = aux xs (x:l)

--Problem 6
isPalindrome :: (Eq a )=>[a] -> Bool
isPalindrome xs = xs == (myReverse2 xs)

--Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten (Elem a)= [a]
flatten (List ls) = concatMap flatten ls

--Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x: ( compress $ dropWhile (==x) xs)

--Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = reverse $ foldl pack' [] xs
  where
    pack' [] x = [[x]]
    pack' (xs:xss) x = if head xs == x then (x:xs):xss
                       else [x]:(xs:xss)

goodpack :: Eq a => [a] -> [[a]]
goodpack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : goodpack rest
goodpack [] = []

--Problem 10
encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode xs = encode' $ goodpack xs
  where
    encode' [] = []
    encode' (xs:xss) = (length xs, head xs): encode' xss

--Problem 11
data Occurrence a = Multiple Int a | Single a
  deriving(Show)
encodeModified :: Eq a => [a] -> [Occurrence a]
encodeModified xs = map occurs $ encode xs
  where
    occurs (n,x)
      | n>1 = Multiple n x
      | otherwise = Single x

--Problem 12
decodeModified :: [Occurrence a] -> [a]
decodeModified = concatMap f
  where
    f (Multiple n x) = replicate n x
    f (Single x) = [x]

--Problem 13
encodeDirect ::Eq a => [a] -> [Occurrence a]
encodeDirect [] = []
encodeDirect xs = foldr go [] xs
  where
    go x [] = [Single x]
    go x os@(o:os') =  case o of 
      Single e -> if e == x then (Multiple 2 e):os'
                  else (Single x):os
      Multiple n e-> if x==e then (Multiple (n+1) e):os'
                      else Single x:os

--Problem 14
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

--Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

--Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n 1
  where
    dropEvery' xs 0 _ = xs
    dropEvery' [] _ _ = []
    dropEvery' (x:xs) n i
      | i==n = dropEvery' xs n 1
      | otherwise = x:dropEvery' xs n (i+1)

--Problem 17
split :: [a] -> Int -> ([a],[a])
split xs 0 = ([],xs)
split [] _ = ([],[])
split (x:xs) n = let (ls,rs) = split xs (n-1) in (x:ls,rs)

--Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs l r =  snd $ split (fst $ split xs r) (l-1)

--Problem 19
rotate :: [a] -> Int -> [a]
rotate xs@(x:xs') n
  | abs(rem n (length xs))==0 = xs
  | n>0 = slice (cycle xs) (paso+1) (len + paso)
  | n<0 = slice (cycle xs) (len-paso+1) (len*2 - paso)
  where
    paso = abs $ rem n len
    len = length xs

--Problem 20

--Problem 21
--Problem 22
--Problem 23
--Problem 24
--Problem 25
--Problem 26
--Problem 27
--Problem 28
--Problem 29
--Problem 30

 --Problem 31
isPrime x = [ d |d<-[2..x `div` 2], d*d==x, x `mod` d == 0] == []

--Problem 32
maxComDiv :: Integer -> Integer -> Integer
maxComDiv a 1 = 1
maxComDiv a 0 = a
maxComDiv a b = maxComDiv b (a `mod` b)

--Problem 33
coprime a b = maxComDiv a b == 1

--Problem 34
totiem 1    = 1
totiem x    = auxfunc x (x-1)
    where 
        auxfunc n 1     = 1
        auxfunc n c     = if (coprime n c) then (1 + auxfunc n (c-1))
                        else auxfunc n (c-1)

--Problem 40

goldbach x = filter bothPrime [(e,x -e)|e<-(2:[3,5..x `div` 2])]
    where
        bothPrime (x,y) = isPrime x && isPrime y

--Problem 41


--Problem 42
--Problem 43
--Problem 44
--Problem 45
--Problem 46
--Problem 47
--Problem 48
--Problem 49
--Problem 50
--Problem 51
--Problem 52
--Problem 53
--Problem 54
--Problem 55
--Problem 56
--Problem 57
--Problem 58
--Problem 59
--Problem 60
--Problem 61
--Problem 62
--Problem 63
--Problem 64
--Problem 65
--Problem 66
--Problem 67
--Problem 68
--Problem 69
--Problem 70
--Problem 71
--Problem 72
--Problem 73
--Problem 74
--Problem 75
--Problem 76
--Problem 77
--Problem 78
--Problem 79
--Problem 80
--Problem 81
--Problem 82
--Problem 83
--Problem 84
--Problem 85
--Problem 86
--Problem 87
--Problem 88
--Problem 89
--Problem 90
--Problem 91
--Problem 92
--Problem 93
--Problem 94
--Problem 95
--Problem 96
--Problem 97
--Problem 98
--Problem 99
