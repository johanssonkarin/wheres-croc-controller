# A Where's Croc Controller

A control system to compete on the Where’s Croc game.

## About The Game
You are a ranger in Wollomunga national park, in outback Australia. Crocodiles in this park have sensors attached that say where they are and the water conditions (salinity, phosphate and nitrogen levels) of the water the crocodile is current swimming in. The park consists of 40 waterholes, each of which is reachable only from its neighbors. The park has records of the calcium, salinity and alkalinity distributions in each waterhole. The sensor on one crocodile, 'Croc', has broken. It no longer says where he is, however it does still provide water condition readings. You need to find Croc. There are also two Swedish backpackers in the park, wandering around at random visiting waterholes. If they end up in the same waterhole as Croc, they will be eaten. You can move a ranger around the waterhole network and search for Croc at different locations. Your score is the number of moves it takes to find Croc.

## The Script
Implements hidden Markov models and associated algorithms, to model the dynamic probabilities of Croc being at different water holes. More specifically the HMM forward algorithm is used and the problilities from each round is pass to the following as a prior. The prior, together with the new readings, is used to calculate the posterior and to find the best next move for the ranger.

The main function takes five arguments:
1. A list of information for the move. This has two fields. The first is a vector of numbers called 'moves', where the moves to make is then entered. It should be entered two moves (to make it possible to move to a neighboring waterhole and search). Valid moves are the numbers of a neighboring or current waterhole or '0' which means searching the current waterhole for Croc. The second field is a list called 'mem' that can be used to store information from turn to turn.
2. A vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current location.
3. A vector giving the positions of the two tourists (elements 1 and 2) and the ranger (element 3). If a tourist has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist was eaten by Croc in a previous turn, then the position will be NA.
4. A two column matrix giving the edges paths between waterholes (edges) present (the numbers are from and to numbers for the waterholes). All edges can be crossed both ways, so are only given once. 
5. A list of three matrices giving the mean and standard deviation of readings for salinity, phosphate and nitrogen respectively at each waterhole.

## Usage
This script needs to be run together with the WheresCroc package, which is written by Mike Ashcroft for the AI course at Uppsala University. Having said that, the tar file is included in the repository.

```R
runWheresCroc(myFunction, doPlot = T, showCroc = T, pause = 1,
               verbose = F, returnMem = F, mem = NA)
```
