# Lamdba world 2018
Jarek Ratajski 

Workshop materials


#  environment check
#  environment check


`> java -version`

Suggested version 8. (example openjdk version 1.8.0_181)

`> etlas -version`

`> eta -version`


##  simple eta project

create file hello.hs with content below:
```
main = putStrLn "Hello, Lambda World 2018"
```

`> eta hello.hs`

`> ls `

`> java -jar Runhello.jar`


## qsort

lets play with sorting:
create file qsort.hs with content below:
```
quicksort [] = []
quicksort (x:xs) = quicksort left ++ [x] ++ quicksort right
    where
          left  = [ y | y <- xs, y < x ]
          right = [ y | y <- xs, y >= x ]

main = do
        let result = quicksort arr
        putStrLn $ show result
        where
            arr = [1,7,9,12,90,1,-1,22,0]

```

`> eta qsort.hs`

`> ls `

`> java -jar Runqsort.jar`


## advanced

- do You have your own haskell project?
- can You implement *real* quick sort?  


# java eta interoperability

clone git project `https://github.com/setblackWs/etaLife.git`

go to main folder

`> ./gradlew :frame:run`

If You get error about missing javafx library you have to install it (on ubuntu sudo apt-get install openjfx)

## exercise

We are doing 
[Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)

If You click on **next** button of **auto**  nothing happens.

Because the generation of a next plane is ... simplified.

```
nextGeneration::Plane->Plane
nextGeneration plane =  plane
```

Check defeintions of `Cell`, `Plane`, `Row` and try to write rules for a game.

>Any live cell with fewer than two live neighbors dies, as if by underpopulation.
 Any live cell with two or three live neighbors lives on to the next generation.
 Any live cell with more than three live neighbors dies, as if by overpopulation.
 Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.


## technical problems
`> ./gradlew clean`  cleans the project

`> ./gradlew :run`  runs only eta part (nice for debug)

## advanced

The game runs relatively slow. 
Can You make it faster?



## TODOS alternative
- better frame
- prepare game pong
- comments
-
