{-
			Perfect Maze Authors: Argyro Ritsogianni - Leonidas Triantafyllou
			Braid Maze Author: Leonidas Triantafyllou
			Implemented in Fall 2016-2017
-}

import System.IO.Unsafe
import System.Random

data Maze = Maze { cells :: [(Bool, Bool)]  -- [(rightWall, downWall)]
                 , width :: Int
                 , height :: Int
                 }

rand :: Int -> Int
-- Returns a random integer from 0 to max-1
rand max = unsafePerformIO $ randomRIO (0, max-1)

shuffle :: [a] -> [a]
-- Randomly shuffles a list
shuffle = unsafePerformIO . shuffleM

shuffleM :: [a] -> IO [a]
-- DON'T BOTHER! Helper for shuffle
shuffleM [] = return []
shuffleM n = do {
                r <- fmap (flip mod $ length n) randomIO;
                n1 <- return $ n !! r;
                fmap ((:) n1) $ shuffleM $ (take r n) ++ (drop (r+1) n)
             }

--ShowMaze
showMaze:: Maze->[(Int,Int)]->String

showMaze (Maze c w h) xs = showMaze1 c xs w h  

showMaze1 cs xs w h = printline w ++ printmaze cs xs w h 0

printline 0 = "+\n"
printline w = "+---" ++ printline (w-1)

printmaze cs xs w h ctr | ctr == h = ""
                        | otherwise  = pright cs xs w 0 ctr ++ pdown cs w h 0 ctr ++ printmaze (droplist cs w) xs w h (ctr+1)

droplist xs 0 = xs
droplist ((x,y):xs) w = droplist xs (w-1)

sol [] m1 m2 = False
sol ((x,y):xs) m1 m2  = if( m1 == y && m2 == x ) then True else sol xs m1 m2

pright ((x,y):cs) xs w m1 m2 | (m1 == 0) && (m1 == w-1) && (sol xs m1 m2 == True) = "| * |\n"
                             | (m1 == 0) && (sol xs m1 m2 == True)   = if( x == True ) then "| * |" ++ pright cs xs w (m1+1) m2
                                                                       else "| *  " ++ pright cs xs w (m1+1) m2
                             | (m1 < w-1) && (sol xs m1 m2 == True)  = if( x == True ) then " * |" ++ pright cs xs w (m1+1) m2
                                                                       else " *  " ++ pright cs xs w (m1+1) m2
                             | (m1 == w-1) && (sol xs m1 m2 == True) = " * |\n"
                             | (m1 == 0) && (m1 == w-1)              = "|   |\n"
                             | (m1 == 0)                             = if( x == True ) then "|   |" ++ pright cs xs w (m1+1) m2
                                                                       else "|    " ++ pright cs xs w (m1+1) m2
                             | (m1 < w-1)                            = if( x == True ) then "   |" ++ pright cs xs w (m1+1) m2
                                                                       else "    " ++ pright cs xs w (m1+1) m2
                             | (m1 == w-1)                           = "   |\n"

pdown ((x,y):cs) w h m1 m2 | (m1 == 0) && (m1 == w-1) = if( m2 == h-1 ) then "+---+\n" else "+   +\n"
                           | (m1 == 0)                = if( y == True ) then "+---+" ++ pdown cs w h (m1+1) m2
                                                        else "+   +" ++ pdown cs w h (m1+1) m2
                           | (m1 < w-1)               = if( y == True ) then "---+" ++ pdown cs w h (m1+1) m2
                                                        else "   +" ++ pdown cs w h (m1+1) m2
                           | (m1 == w-1)              = if( y == True ) then "---+\n" else "   +\n"

makeMaze:: Int->Int->Maze

makeMaze w h = Maze (makeMaze1 (w*h)) w h
makeMaze1 0 = []
makeMaze1 n = [(True,True)] ++ makeMaze1 (n-1)

--kruskal
kruskal :: Maze -> Maze

kruskal (Maze c w h) = Maze (forloop (shuffle (walls w h 0 0) ) (createsets (w*h) 0) c w) w h
printi (Maze c w h) = c

createsets 0 m = []
createsets n m = [[m]] ++ createsets (n-1) (m+1)

walls w h c1 c2 | (c1 == w-1) && (c2 == h-1) = []
                | (c1 == w-1) = [(c1+c2*w,c1+c2*w+w)] ++ walls w h 0 (c2+1)
                | (c2 == h-1) = [(c1+c2*w,c1+c2*w+1)] ++ walls w h (c1+1) c2
                | otherwise = [(c1+c2*w,c1+c2*w+1)] ++ [(c1+c2*w,c1+c2*w+w)] ++ walls w h (c1+1) c2

forloop [] sets mz w = mz
forloop ((c1,c2):xs) sets mz w = if ((not (mymember c1 (myselect (c2+1) sets)))&&(not (mymember c2 (myselect (c1+1) sets)))) 
then forloop xs (joinset (myunion (myselect (c1+1) sets) (myselect (c2+1) sets)) (myunion (myselect (c1+1) sets) (myselect (c2+1) sets)) sets) (connect c1 mz (c2-c1) w) w
else forloop xs sets mz w

connect c1 mz b w = mychange c1 0 b mz w 

mychange _ _ _ [] _ = []
mychange c1 n b ((x,y):mz) w | w == 1    = if(c1==n) then ((y,False):(mychange c1 (n+1) b mz) w) else ((x,y):(mychange c1 (n+1) b mz) w)
                             | b == 1    = if(c1==n) then ((False,y):(mychange c1 (n+1) b mz) w) else ((x,y):(mychange c1 (n+1) b mz) w)
                             | otherwise = if(c1==n) then ((x,False):(mychange c1 (n+1) b mz) w) else ((x,y):(mychange c1 (n+1) b mz) w)

joinset [] _ sets = sets
joinset (n:ns) us sets =  joinset ns us (mydrop n 0 sets us)
 
myunion [] []= []
myunion [] (y:ys)= y:(myunion [] ys)
myunion (x:xs) ys = x:(myunion xs ys)

mydrop _ _ [] _ = []
mydrop n  c (x:xs) us = if(c==n) then (us:(mydrop n (c+1) xs us)) else (x:(mydrop n (c+1) xs us))

myselect 1     (x:xs) = x
myselect n (x:xs) = myselect (n-1) xs

mymember x []     = False
mymember x (y:ys) = if (x==y) then True
else (mymember x ys)

nowalls [] w h metr = []
nowalls ((x,y):cs) w h metr | x == False && y == False = [(metr,metr+1),(metr,metr+w)] ++ nowalls cs w h (metr+1)
                            | x == False               = [(metr,metr+1)] ++ nowalls cs w h (metr+1)
                            | y == False               = [(metr,metr+w)] ++ nowalls cs w h (metr+1)
                            | otherwise                = nowalls cs w h (metr+1)

adjacent _ [] _ _ = []
adjacent (x,y) ((a,b):nws) w h | x*w+y == a && (w==1) = [(b `mod` h,0)]  ++ adjacent (x,y) nws w h
                               | x*w+y == b && (w==1) = [(a `mod` h,0)]  ++ adjacent (x,y) nws w h
                               | x*w+y == a && (h==1) = [(0,b `mod`w)]  ++ adjacent (x,y) nws w h
                               | x*w+y == b && (h==1) = [(0,a `mod` w)]  ++ adjacent (x,y) nws w h 
                               | x*w+y == a = [(b `div`w,b `mod` w)]  ++ adjacent (x,y) nws w h
                               | x*w+y == b = [(a `div`w,a `mod` w)]  ++ adjacent (x,y) nws w h
                               | otherwise = adjacent (x,y) nws w h

--solvePerfect
solvePerfect :: Maze ->(Int, Int) ->(Int, Int) ->[(Int, Int)]
solvePerfect (Maze cs w h) (sx,sy) (gx,gy) | sx == gx && sy == gy = [(sx,sy)]
 | otherwise = solveperfect1 cs w h [(sx,sy)] (adjacent (sx,sy) (nowalls cs w h 0) w h) (sx,sy) (nowalls cs w h 0) (gx,gy)

solveperfect1 cs w h ls [] (prevx,prevy) nwls (gx,gy) = []
solveperfect1 cs w h ls ((curx,cury):st) (prevx,prevy) nwls (gx,gy) | (curx==gx)&&(cury==gy) = ls++[(gx,gy)]
                                                                    | otherwise = 
 solveperfect1 cs w h (ls++[(curx,cury)]) (dropadjacent (adjacent (curx,cury) nwls w h) (prevx,prevy)) (curx,cury) nwls (gx,gy) 
 ++ solveperfect1 cs w h (ls) st (prevx,prevy) nwls (gx,gy)

dropadjacent [] _ = []
dropadjacent ((x,y):xs) (px,py) | x == px && y == py = dropadjacent xs (px,py)
                                | otherwise = (x,y):dropadjacent xs (px,py)

--Run the maze or solve the maze you want
runmaze w h = putStr ( showMaze (kruskal (makeMaze w h)) [])
solvemaze w h (sx,sy) (gx,gy) = solvemaze1 (kruskal (makeMaze w h)) (sx,sy) (gx,gy)
solvemaze1 maze (x1,y1) (x2,y2) = putStr ( showMaze maze (solvePerfect maze (x1,y1) (x2,y2)))

--bonus (Braid Maze)
braid :: Maze ->Maze

braid (Maze cs w h) = Maze (braid1 cs cs w h 0 0 ) w h

braid1 [] cs _ _ _ _ = cs
braid1 ((x,y):xs) cs w h m1 m2 | m1 /= h-1 && m2 /= w-1 = if(countadj (adjacent (m1,m2) (nowalls cs w h 0) w h) == 1) then
                                                             braid1 xs (wchange cs m1 m2 w h 0 0) w h m1 (m2+1)
                                                          else braid1 xs cs w h m1 (m2+1)
                               | m1 /= h-1 && m2 == w-1 = if(countadj (adjacent (m1,m2) (nowalls cs w h 0) w h) == 1) then (
                                                             if(y == False) then
                                                                 braid1 xs (wchange cs m1 (m2-1) w h 0 0) w h (m1+1) 0
                                                             else braid1 xs (wchange cs m1 m2 w h 0 0) w h (m1+1) 0 )
                                                          else braid1 xs cs w h (m1+1) 0
                               | m1 == h-1 && m2 == w-1 = braid1 xs (rem2 cs w h 0 0) w h m1 m2
                               | m1 == h-1              = if(countadj (adjacent (m1,m2) (nowalls cs w h 0) w h) == 1 ) then (
                                                             if(x == False) then
                                                                 braid1 xs (wchange cs (m1-1) m2 w h 0 0) w h m1 (m2+1)
                                                             else braid1 xs (wchange cs m1 m2 w h 0 0) w h m1 (m2+1) )
                                                          else braid1 xs cs w h m1 (m2+1)

wchange [] _  _ _ _ _ _ = []
wchange ((x,y):xs) m1 m2 w h t1 t2 | t2/=w-1 = if( m1 == t1 && m2 == t2) then (rem1 (x,y) m1 h):wchange xs m1 m2 w h t1 (t2+1)
                                               else (x,y):wchange xs m1 m2 w h t1 (t2+1)
                                   | t2==w-1 = if( m1 == t1 && m2 == t2) then (rem1 (x,y) m1 h):wchange xs m1 m2 w h (t1+1) 0
                                               else (x,y):wchange xs m1 m2 w h (t1+1) 0
countadj [] = 0
countadj ((x,y):xs) = 1 + countadj xs

rem1 (x,y) m1 h | y == True && m1 /= h-1 = (x,False)
                | x == True && m1 /= h-1 = (False,y)
                | x == True && m1 == h-1 = (False,y)
                | y == True && m1 == h-1 = (x,False)

rem2 [] _ _ _ _ = []
rem2 ((x,y):xs) w h t1 t2 | t1 == h-1 && t2 == w-2 = (False,y):rem2 xs w h t1 (t2+1)
                          | t1 == h-2 && t2 == w-1 = (x,False):rem2 xs w h (t1+1) 0
                          | t2 == w-1              = (x,y):rem2 xs w h (t1+1) 0
                          | otherwise              = (x,y):rem2 xs w h t1 (t2+1)

--solve braid
solveBraid :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]

solveBraid (Maze cs w h) (sx,sy) (gx,gy) | sx == gx && sy == gy = [(sx,sy)]
 | otherwise            = solb (solbraid1 cs w h [(sx,sy)] (adjacent (sx,sy) (nowalls cs w h 0) w h) (sx,sy) (nowalls cs w h 0) (gx,gy) ) (gx,gy)


solbraid1 cs w h ls [] (prevx,prevy) nwls (gx,gy) = []
solbraid1 cs w h ls ((curx,cury):st) (prevx,prevy) nwls (gx,gy) | (curx==gx)&&(cury==gy) = ls++[(gx,gy)]
 | otherwise              = solbraid1 cs w h (ls++[(curx,cury)]) (dropadjacent1 (revL (adjacent (curx,cury) nwls w h)) (prevx,prevy) ls) (curx,cury) nwls (gx,gy) ++
                            solbraid1 cs w h (ls) st (prevx,prevy) nwls (gx,gy)

dropadjacent1 [] _ _ = []
dropadjacent1 ((x,y):xs) (px,py) vis | x == px && y == py = dropadjacent1 xs (px,py) vis
                                     | find vis (x,y) == True = dropadjacent1 xs (px,py) vis
                                     | otherwise = (x,y):dropadjacent1 xs (px,py) vis

find [] _ = False
find ((x,y):xs) (cx,cy) | x == cx && y == cy = True
                        | otherwise = find xs (cx,cy)

solb ((x,y):xs) (gx,gy) | x == gx && y == gy = [(x,y)]
                        | otherwise =(x,y): solb xs (gx,gy)

revL [] = []
revL (x:xs) = revL xs ++ [x]
--print braid or solve braid

printbraid w h = putStr (showMaze (braid (kruskal (makeMaze w h) ) ) [] )

solvebraid1 maze (x1,y1) (x2,y2) = putStr ( showMaze maze (solveBraid maze (x1,y1) (x2,y2)))

solvebraid w h (sx,sy) (gx,gy) = solvebraid1 (braid (themaze w h)) (sx,sy) (gx,gy)

--print both or solve both

themaze w h = (kruskal (makeMaze w h))

printboth1 maze = putStr ( showMaze maze [] ++ "\n\n" ++ showMaze (braid maze) [] )

braidsol maze (sx,sy) (gx,gy) = showMaze maze (solveBraid maze (sx,sy) (gx,gy) )

solveboth1 maze (sx,sy) (gx,gy) = putStr ( ( (showMaze maze (solvePerfect maze (sx,sy) (gx,gy)))) ++ "\n\n" ++ ( braidsol (braid maze) (sx,sy) (gx,gy) ) )

printboth w h = printboth1 (themaze w h)

solveboth w h (sx,sy) (gx,gy) = solveboth1 (themaze w h) (sx,sy) (gx,gy)
