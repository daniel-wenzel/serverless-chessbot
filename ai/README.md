Hi guys,
yes we are almost done. Only a few things remain. 

execute this project by opening a terminal in the git directory and running
```
ghci test3.hs
```
(at least on linux)

As soons as you change something you have to reload the file with

```
:r
```

To find all allowed moves execute
```
getAllAllowedMoves White testBoard 
```

To find out where you can go to from a given plot execute
```
canMoveToPlots (getPlotAt 4 6 testBoard ) testBoard 
```