require(data.table)
require(rgl)
library(ggplot2)
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

plotSubspace <- function(data, subspace, outlierClass){
  if(length(subspace) > 3){
    stop("cannot plot subspace with more than 3 dimensions")
  }else if(length(subspace) == 3){
    plot3d(subset(data[data$class!=outlierClass], select=subspace), size=6)
    plot3d(subset(data[data$class==outlierClass], select=subspace), size=6, col="red", add=T)
  }else{
    plot(subset(data[data$class!=outlierClass], select=subspace), pch=16)
    points(subset(data[data$class==outlierClass], select=subspace), pch=16, col="red")
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
                            numNonRelevantDim, 
                            numObjects, 
                            minSubspaceSize, 
                            maxSubspaceSize, 
                            numOutliersPerSubspace, 
                            intervals, 
                            symmetricProportion){
  if((symmetricProportion<1) & !(length(intervals) > 1)){
    stop("if asymmetric provide at least two intervals")
  }
  
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
  subspaceType <- vector(mode="list")
  labels <- rep(0, numObjects)
  for(i in 1:length(subspaces)){
    s <- subspaces[[i]]
    print(s)
    asymmetricAttribute <- sample(1:length(s), 1)
    sym <- runif(1)<symmetricProportion
    subspaceType[i] <- ifelse(sym, "symmetric", "asymmetric")
    objects <- do.call(rbind, replicate(numObjects, generateObject(intervals = intervals, 
                                                                   symmetric = sym, 
                                                                   dim = length(s), 
                                                                   asymmetricAttribute, 
                                                                   "inlier"), simplify = F))
    
    outliers <- do.call(rbind, replicate(numOutliersPerSubspace, generateObject(intervals = intervals, 
                                                                                symmetric = sym, 
                                                                                dim = length(s), 
                                                                                asymmetricAttribute, 
                                                                                "outlier"),  simplify = F))
    
    outlierIndices <- sample(1:nrow(objects), numOutliersPerSubspace)
    
    objects[outlierIndices,]  <- outliers
    labels[outlierIndices] <- i
    randomData <- as.data.table(cbind(randomData, objects))
  }
  
  ## ---- end
  
  ## ---- generateNonRelevantDimensions
  if(numNonRelevantDim > 0){
    for(i in 1:(numNonRelevantDim/2)){
      subspaces[[length(subspaces)+1]] <- c(numRelevantDim + i, numRelevantDim + i + 1)
      subspaceType[length(subspaceType)+1] <- "irrelevant"
      x1 <- runif(numObjects)
      x2 <- sapply(x1, function(z) z + rnorm(1, 0, 0.01))
      randomData <- cbind(randomData, x1, x2)
    }  
  }
  
  ## ---- end
  
  randomData <- cbind(randomData, labels)
  
  ## ---- renameData
  if(numNonRelevantDim > 0){
    names(randomData) <- c(paste0("var_",formatC(1:numRelevantDim, width=4, format="d", flag="0")),
                         paste0("noise_",formatC(1:numNonRelevantDim, width=4, format="d", flag="0")),
                         "class")
  }
  else{
    names(randomData) <- c(paste0("var_",formatC(1:numRelevantDim, width=4, format="d", flag="0")), "class")
  }
  ## ---- end
  
  print(paste("number of generated outliers: ", nrow(randomData[randomData$class>0])))
  print(paste("number of generated subspaces: ", length(subspaces)))
  
  return (list("data"=randomData, "subspaces"=subspaces, "subspaceTypes"=subspaceType))
}
