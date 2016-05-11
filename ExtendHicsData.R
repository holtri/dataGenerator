require(data.table)
require(rgl)

## ---- helperFunctions

inInterval <- function(intervals, x){
  any(sapply(intervals, function(a) a[1] < x & a[2] > x))
}

allButOne <-function(v) {
  length(v) - sum(v) == 1
}
exactlyOne <- function(v){
  length(v) == 11
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

## ---- end

generateDataSet <- function(datasetNumber, 
                            numRelevantDim, 
                            numNonRelevant, 
                            numObjects, 
                            minSubspaceSize, 
                            maxSubspaceSize, 
                            numOutliersPerSubspace, 
                            intervals, 
                            symmetric){
  
  
  ## ---- objectGeneration
  generateObject <- function(intervals, symmetric, dim, asymmetricAttribute, type = "inlier"){
    asymmetricCondition <- FALSE
    repeat{
      dataObject <- runif(dim)
      tmp <- sapply(dataObject, function(x) inInterval(intervals, x))  
      if(!symmetric){
        asymmetricCondition <- inInterval(intervals[1], dataObject[asymmetricAttribute])
        
      }
      switch(type,
        outlier = {
          if(!asymmetricCondition && (all(tmp) | exactlyOne(tmp))){
            return (dataObject)
          }      
        },
        inlier={
          if(asymmetricCondition || (!any(tmp) | allButOne(tmp))){
            return (dataObject)
          }
        }
      )
    }
  }
  ## ---- end
  
  randomData <- as.data.table(matrix(runif(numObjects*numRelevantDim), numObjects, numRelevantDim))
  
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
  
  ## ---- generateRandomObjects
  randomData <- c()
  labels <- rep(0, numObjects)
  for(i in 1:length(subspaces)){
    s <- subspaces[[i]]
    print(s)
    asymmetricAttribute <- sample(1:length(s), 1)
    objects <- do.call(rbind, replicate(numObjects, generateObject(intervals = intervals, 
                                                                   symmetric = symmetric, 
                                                                   dim = length(s), 
                                                                   asymmetricAttribute, 
                                                                   "inlier"), simplify = F))
    
    outliers <- do.call(rbind, replicate(numOutliersPerSubspace, generateObject(intervals = intervals, 
                                                                                symmetric = symmetric, 
                                                                                dim = length(s), 
                                                                                asymmetricAttribute, 
                                                                                "outlier"),  simplify = F))
    
    outlierIndices <- sample(1:nrow(objects),4)
    
    objects[outlierIndices,]  <- outliers
    labels[outlierIndices] <- i
    randomData <- as.data.table(cbind(randomData, objects))
  }
  
  ## ---- end
  
  ## ---- generateNonRelevantDimensions
  for(i in 1:(numNonRelevant /2)){
    x1 <- runif(numObjects)
    x2 <- sapply(x1, function(z) z + rnorm(1, 0, 0.01))
    randomData <- cbind(randomData, x1, x2)
  }
  
  ## ---- end
  
  randomData <- cbind(randomData, labels)
  names(randomData) <- c(paste0("var_",formatC(1:numRelevantDim, width=4, format="d", flag="0")),
                         paste0("noise_",formatC(1:numNonRelevant, width=4, format="d", flag="0")),
                         "class")
  print(paste("number of generated outliers: ", nrow(randomData[randomData$class>0])))
  print(paste("number of generated subspaces: ", length(subspaces)))
#  write.table(randomData, file=paste0("synth_multidim_", numNonRelevant + numRelevantDim, "_00", datasetNumber, "_labeled.csv"), sep=";")
#  invisible(lapply(subspaces, write, paste0("synth_multidim_",numNonRelevant + numRelevantDim, "_00", datasetNumber, "_labeled.info"), append=TRUE, ncolumns=1000))
  
  return (list("data"=randomData, "subspaces"=subspaces))
}

