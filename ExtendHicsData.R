require(data.table)

datasetNumber <- 7
numRelevantDim <- 150
numNonRelevant <- 50
numObjects <- 2000
maxSubspaceSize <- 6
minSubspaceSize <- 2
numOutliersPerSubspace <- 4

randomData <- as.data.table(matrix(runif(numObjects*numRelevantDim), numObjects, numRelevantDim))

counter <- 1
subspaces <- vector(mode="list")

while(counter <= numRelevantDim - 2*minSubspaceSize ) {
  tmp <- ceiling(runif(1, minSubspaceSize - 2, min(maxSubspaceSize, numRelevantDim - counter - minSubspaceSize) -1))
  subspaces[[length(subspaces)+1]] <- c(counter:(counter+tmp))
  counter <- counter + tmp + 1
}

subspaces[[length(subspaces)+1]] <- c(counter:numRelevantDim)

for(i in 1:(numNonRelevant /2)){
  x1 <- runif(numObjects)
  x2 <- sapply(x1, function(z) z + rnorm(1, 0, 0.01))
  randomData <- cbind(randomData, x1, x2)
}

names(randomData) <- paste0("var_",formatC(1:ncol(randomData), width=4, format="d", flag="0"))

#create correlated subspaces
for(s in subspaces){
  for(i in 1:numObjects){
    j <- as.integer(ceiling(runif(1, (s[1] -1), s[length(s)])))
    set(randomData, i=i, j=j, value=runif(1, -0.01, 0.01))
    # set(randomData, i=i, j=j, value=runif(1, 0, 0.2))
    # set(randomData, i=i, j=j, value=max(0,randomData[[i,j]] - runif(1, 0, 0.2)))
  }
}
randomData$class <- 0

# place outliers
for(s in subspaces){
  for(i in 1:numOutliersPerSubspace){
    randomObjectIndex <- ceiling(runif(1, 0, numObjects))
    randomData[randomObjectIndex, class:=1]
    randomData[randomObjectIndex, names(randomData[,s, with=F]) := lapply(.SD[,s, with=F], function(x) runif(1, 0.6, 1))]
  }
}

numberOutliers <- nrow(randomData[randomData$class==1])

subspaces
numberOutliers
write.table(randomData, file=paste0("synth_multidim_", numNonRelevant + numRelevantDim, "_00", datasetNumber, "_labeled.csv"), sep=";")
lapply(subspaces, write, paste0("synth_multidim_",numNonRelevant + numRelevantDim, "_00", datasetNumber, "_labeled.info"), append=TRUE, ncolumns=1000)
