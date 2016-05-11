require(data.table)
require(rgl)

# parameter ---------------------------------------------------------------

datasetNumber <- '7'

numRelevantDim <- 30
numNonRelevant <- 10

numObjects <- 1000

minSubspaceSize <- 2
maxSubspaceSize <- 3
numOutliersPerSubspace <- 4
intervals <- list(c(0, 0.3), c(0.6, 0.8))
# intervals <- list(c(0, 0.2))

symmetric <- F

# helper functions ---------------------------------------------------------------

inInterval <- function(intervals, x){
  any(sapply(intervals, function(a) a[1] < x & a[2] > x))
}

allButOne <-function(v) {
  length(v) - sum(v) ==1
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

generateValueNotInInterval <- function(intervals){
  repeat{
    tmp <- runif(1, 0, 1)
    if(!inInterval(intervals, tmp)){
      return(tmp)
    } 
  }
}

generateInlier <- function(intervals, symmetric, dim, asymmetricAttribute = 1){
  
  asymmetricCondition <- FALSE
  repeat{
    dataObject <- runif(dim)
    tmp <- sapply(dataObject, function(x) inInterval(intervals, x))  
    if(!symmetric){
      asymmetricCondition <- inInterval(intervals[1], dataObject[asymmetricAttribute])
      
    }
    if(asymmetricCondition || (!any(tmp) | allButOne(tmp)) ){
      return (dataObject)
    }
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
  tmp <- ceiling(runif(1, minSubspaceSize - 2, 
                       min(maxSubspaceSize, numRelevantDim - counter - minSubspaceSize) -1))
  subspaces[[length(subspaces)+1]] <- c(counter:(counter+tmp))
  counter <- counter + tmp + 1
}
subspaces[[length(subspaces)+1]] <- c(counter:numRelevantDim)
## ---- end

randomData <- c()
for(s in subspaces){
  print(s)
  asymmetricAttribute <- sample(1:length(s), 1)
  randomData <- cbind(randomData, 
                      do.call(rbind, replicate(numObjects, 
                                               generateInlier(intervals = intervals, 
                                                              symmetric = symmetric, 
                                                              dim = length(s), 
                                                              asymmetricAttribute), simplify = F)))
}
randomData <- as.data.table(randomData)

# #create correlated subspaces
# progress <- 0
# for(s in subspaces){
#   asymmetricAttribute <- sample(s, 1, replace = T)
#   print(paste("subspace", progress, "of", length(subspaces)))
#   progress <- progress + 1
#   for(i in 1:numObjects){
#     
#     if(symmetric){
#       tmp <- sapply(randomData[i, s, with=F], function(x) inInterval(intervals, x))
#       while(!(!any(tmp) | allButOne(tmp))){
#         for(dim in s){
#           set(randomData, i=i, j=dim, value=runif(1, 0, 1))
#         }
#         tmp <- sapply(randomData[i, s, with=F], function(x) inInterval(intervals, x))
#       }
#     }else{
#       
#       tmp <- sapply(randomData[i, s, with=F], function(x) inInterval(intervals, x))
#       
#       while(!(inInterval(intervals[1], randomData[[i, asymmetricAttribute]]) || 
#               (!any(tmp) | allButOne(tmp)))){
#         for(dim in s){
#           set(randomData, i=i, j=dim, value=runif(1, 0, 1))
#         }
#         
#         tmp <- sapply(randomData[i, s, with=F], function(x) inInterval(intervals, x))
#       }
#     }
#   }
# }
randomData$class <- 0



# place outliers
for(s in subspaces){
  for(i in 1:numOutliersPerSubspace){
    randomObjectIndex <- ceiling(runif(1, 0, numObjects))
    randomData[randomObjectIndex, class:=1]
    # randomData[randomObjectIndex, names(randomData[,s, with=F]) := lapply(.SD[,s, with=F], function(x) runif(1, 0.6, 1))]
    randomData[randomObjectIndex, names(randomData[,s, with=F]) := lapply(.SD[,s, with=F], function(x) generateValueNotInInterval(intervals))]
  }
}

numberOutliers <- nrow(randomData[randomData$class==1])

print(paste("number of generated outliers: ", numberOutliers))
print(paste("number of generated subspaces: ", length(subspaces)))
write.table(randomData, file=paste0("synth_multidim_", numNonRelevant + numRelevantDim, "_00", datasetNumber, "_labeled.csv"), sep=";")
invisible(lapply(subspaces, write, paste0("synth_multidim_",numNonRelevant + numRelevantDim, "_00", datasetNumber, "_labeled.info"), append=TRUE, ncolumns=1000))
