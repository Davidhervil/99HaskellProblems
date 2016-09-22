
--Problem 1
mylast :: [a] -> a
mylast []  		= error "Lista vacia"
mylast [x] 		= x
mylast (_:xs) 	= mylast xs

mylast1 [x] 	= Just x
mylast1 [] 		= Nothing
mylast1 (_:xs)	= mylast1 xs

mylast2 [x] 	= Right x
mylast2 [] 		= Left "Lista vacia"
mylast2 (_:xs) 	= mylast2 xs

--Problem 2
myButLast :: [a] -> a
myButLast [x] 	= error "No hay suficientes elementos"
myButLast [] 	= error "Lista vacia"
myButLast xs 	= if length xs == 2 then head xs else myButLast (tail xs)

--Problem 3
elementAt (e:_) 0	 = e
elementAt [] _	 = error "Indice fuera de rango"
elementAt (_:xs) n = if n<0 then error "Indice negativo" else elementAt xs (n-1)

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
totiem 1 	= 1
totiem x 	= auxfunc x (x-1)
	where 
		auxfunc n 1 	= 1
		auxfunc n c 	= if (coprime n c) then (1 + auxfunc n (c-1))
	 					else auxfunc n (c-1)

--Problem 40

goldbach x = filter bothPrime [(e,x -e)|e<-(2:[3,5..x `div` 2])]
	where
		bothPrime (x,y) = isPrime x && isPrime y
