require(data.table)
require(rgl)

# parameter ---------------------------------------------------------------

datasetNumber <- '7'

numRelevantDim <- 150
numNonRelevant <- 50

numObjects <- 2000

minSubspaceSize <- 2
maxSubspaceSize <- 6
numOutliersPerSubspace <- 4
# intervals <- list(c(0, 0.2), c(0.5, 0.8))
intervals <- list(c(0, 0.2))
symmetric <- F
# intervals <- list(c(0, 0.1), c(0.2, 0.3), c(0.4, 0.5), c(0.5, 0.6), c(0.7, 0.8), c(0.9, 1)

# functions ---------------------------------------------------------------

inInterval <- function(intervals, x){
  any(sapply(intervals, function(a) a[1] < x & a[2] > x))
}

plotSubspace <- function(data, subspace){
  if(length(subspace) > 3){
    stop("cannot plot subspace with more than 3 dimensions")
  }else if(length(subspace) == 3){
    plot3d(subset(data, select=subspace))
  }else{
    plot(subset(data, select=subspace))
  }
}

randomData <- as.data.table(matrix(runif(numObjects*numRelevantDim), numObjects, numRelevantDim))

## ---- generateNonRelevantDimensions
for(i in 1:(numNonRelevant /2)){
  x1 <- runif(numObjects)
  x2 <- sapply(x1, function(z) z + rnorm(1, 0, 0.01))
  randomData <- cbind(randomData, x1, x2)
}
names(randomData) <- paste0("var_",formatC(1:ncol(randomData), width=4, format="d", flag="0"))

## ---- end

## ---- generateRandomSubspaces
counter <- 1
subspaces <- vector(mode="list")

while(counter <= numRelevantDim - 2*minSubspaceSize ) {
  tmp <- ceiling(runif(1, minSubspaceSize - 2, min(maxSubspaceSize, numRelevantDim - counter - minSubspaceSize) -1))
  subspaces[[length(subspaces)+1]] <- c(counter:(counter+tmp))
  counter <- counter + tmp + 1
}
subspaces[[length(subspaces)+1]] <- c(counter:numRelevantDim)
## ---- end

#create correlated subspaces
for(s in subspaces){
  for(i in 1:numObjects){
    if(symmetric){
      j <- as.integer(ceiling(runif(1, (s[1] - 1), s[length(s)])))
      updateCol <- j
    }
    else{
      j <- as.integer(ceiling(runif(1, (s[1] - 1), s[length(s)] - 1)))   
      updateCol <- s[length(s)]
    }
    while(!inInterval(intervals, randomData[[i,updateCol]])){
      set(randomData, i=i, j=updateCol, value=runif(1, 0, 1))
    }
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

print(paste("number of generated outliers: ", numberOutliers))
print(paste("number of generated subspaces: ", length(subspaces)))
write.table(randomData, file=paste0("synth_multidim_", numNonRelevant + numRelevantDim, "_00", datasetNumber, "_labeled.csv"), sep=";")
invisible(lapply(subspaces, write, paste0("synth_multidim_",numNonRelevant + numRelevantDim, "_00", datasetNumber, "_labeled.info"), append=TRUE, ncolumns=1000))
