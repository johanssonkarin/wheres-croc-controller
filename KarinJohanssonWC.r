library(WheresCroc)

findMatchingNodeIndex = function(theList, neighbor){
  if(length(theList)==0){return(0)}
  index = which(sapply(theList,function(node)(node$currentPos==neighbor$currentPos)))
  ifelse(length(index)==0, return(0), return(index))
}

calculateNeighbors = function(parent, goal, edges, frontier, visited){
  connecting_col1 = which(edges[,1]==parent$currentPos)
  conneting_col2 = which(edges[,2]==parent$currentPos)
  connecting_nodes = sapply(connecting_col1, function(i) edges[i,2])
  connecting_nodes <- append(connecting_nodes, unlist(sapply(conneting_col2, function(i) edges[i,1])))
  
  for(node_i in connecting_nodes){
    node = list(currentPos = node_i,
              path = append(node_i, parent$path),
              f = length(parent$path) +1
              )
  index = findMatchingNodeIndex(frontier,node)
  if(index == 0){
    if(!(node_i %in% visited)){
      frontier <- append(list(node),frontier)
    } 
  }
}

return(frontier)
}

createPathList = function(startPos, goalPos, edges){
  node = list(currentPos = startPos, 
              path = list(),
              f = 0)
  frontier = list(node)
  visited <- c()
  repeat{
    node_costs = sapply(frontier,function(node) node$f)
    best_index = which.min(node_costs)
    node_to_expand = frontier[[best_index]]
    if(node_to_expand$currentPos == goalPos){break} #Stop if the goal node is about to be expanded
    frontier <- calculateNeighbors(node_to_expand, goalPos, edges, frontier[-best_index],visited)
    visited <- unique(c(visited,node_to_expand$currentPos))
  } 
  scores=sapply(frontier,function(node)node$f)
  best_index=which.min(scores)
  return(frontier[[best_index]]$path) #outputs path backwards
}

createTransitionMatrix <- function(edges){
  transition_matrix <- matrix(0,nrow = 40, ncol = 40)
  for(i in 1:length(edges[,1])){
    transition_matrix[edges[i,1],edges[i,2]] <- 1
    transition_matrix[edges[i,2],edges[i,1]] <- 1
  }
  for (i in 1:40) {
    transition_matrix[i,i] <-1
  }
  transition_matrix <- (transition_matrix/rowSums(transition_matrix))
  return(transition_matrix)
}

createObservationMatrix <- function(readings,probs){
  observation_matrix <- matrix(0,nrow = 40, ncol = 40)
  probS <- probs$salinity
  probP <- probs$phosphate
  probN <- probs$nitrogen
  crocS <- readings[1]
  crocP <- readings[2]
  crocN <- readings[3]
  
  for(waterhole in 1:40){ #calculating probability denisty for each waterhole
    densityS = dnorm(crocS, probS[waterhole, 1], probS[waterhole, 2])
    densityP = dnorm(crocP, probP[waterhole, 1], probP[waterhole, 2])
    densityN = dnorm(crocN, probN[waterhole, 1], probN[waterhole, 2])
    observation_matrix[waterhole, waterhole] = densityS*densityP*densityN
  }
  return(observation_matrix)
}

#the state vector, representing the probability of each waterhole
createStateVector <- function(positions, state_vector, transition_matrix, observation_matrix){
  s <- state_vector

  ifelse(is.na(positions[1]), #check if backpacker 1 is eaten and adjust state accordingly
         s[1,positions[1]] <- 0,
         if (positions[1] < 0){
           s[1,abs(positions[1])] <-1 #zero out all but backpacker position
           s[s != 1] <- 0}
         else{s[1,positions[1]] <- 0}) 

 ifelse(is.na(positions[2]), #check if backpacker 2 is eaten and adjust state accordingly
        s[1,positions[2]] <- 0,
        if (positions[2] < 0){
            s[1,abs(positions[2])] <-1 #zero out all but backpacker position
            s[s != 1] <- 0}
            else{s[1,positions[2]] <- 0})
 
  s <- s%*%transition_matrix%*%observation_matrix
  s <- (s/rowSums(s))
  return(s)
}


myFunction = function(moveInfo,readings,positions,edges,probs) {

  if(moveInfo$mem$status == 0){ 
    state_vector <- matrix(1/39, nrow = 1, ncol = 40)
    state_vector[positions[3]] <- 0 #croc cannot start where ranger starts
    
    transition_matrix <- createTransitionMatrix(edges)
    
    #precalculate what different moves to make in different situations
    moves2make = matrix(list(), nrow = 40, ncol = 40)
    for (r in 1:40){#row = ranger position, column = croc position
      for (c in 1:40){
        moves2make[r,c] <- list(createPathList(r,c,edges))}}
    
    moveInfo$mem$state <- state_vector #save
    moveInfo$mem$status <- 2  
    moveInfo$mem$transition <- transition_matrix
    moveInfo$mem$moves2make <- moves2make
  }
  else if(moveInfo$mem$status==1){ 
    moveInfo$mem$status <- 2 
    state_vector <- matrix(1/39, nrow = 1, ncol = 40)
    state_vector[positions[3]] <- 0 #croc cannot start where ranger starts
    transition_matrix <- moveInfo$mem$transition
  }
  
  else{ 
    state_vector <- moveInfo$mem$state
    transition_matrix <- moveInfo$mem$transition
  }
    
  observation_matrix <- createObservationMatrix(readings, probs)
  state_vector <- createStateVector(positions, state_vector, transition_matrix, observation_matrix)
  crocPos <- which.max(state_vector)
  path <- rev(unlist(moveInfo$mem$moves2make[positions[3],crocPos]))

  #choose moves
  if(length(path) < 1){#no more moves, just search
    moveInfo$moves <- c(0,0)
    state_vector[crocPos] <- 0
  }

  else if(length(path) < 2) {
    moveInfo$moves <-c(path[[1]], 0)
  }
  
  else{
    moveInfo$moves <-c(path[[1]],path[[2]])
  }
  
  moveInfo$mem$state <- state_vector
  return(moveInfo)
}

#runWheresCroc(myFunction, doPlot = T, showCroc = T, pause = 1,verbose = F, returnMem = F, mem = NA)
#testWC(myFunction, verbose = 0, returnVec = FALSE, n = 500, seed = 21, timeLimit = 300)

