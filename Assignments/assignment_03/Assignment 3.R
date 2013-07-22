#install.packages("e1071",lib = "Rpackages")
library("e1071")
library(class)
library(ggplot2)
data <- iris
labels <- data$Species
data$Species <- NULL
set.seed(1)

# function to run naive bayes on a train-test pair. it returns the error
nb.errf <- function(train.data, test.data, train.labels, test.labels){
  
  classifier <- naiveBayes(train.data, train.labels)
  nb.prediction <- predict(classifier, test.data)
  this.err <- sum(test.labels != nb.prediction)/length(test.labels)
  
  return(this.err)
}

#function to extract the k-th subset of data and labels
#and to run nb.errf on the pair. It returns the prediction error.
nb.k.errf <- function(k, whichK, data, labels){
  train.data <- data[whichK != k,]
  test.data <- data[whichK == k,]
  train.labels <- labels[whichK != k]
  test.labels <- labels[whichK == k]
  this.err.k <- nb.errf(train.data, test.data, train.labels, test.labels)
  return(this.err.k)
}

# function that builds a vector index to generate the CV subsamples.
# Code from: https://gist.github.com/dsparks/3695362
build.whichK <- function(kk, data){
  randomDraw <- rnorm(nrow(data))
  kQuantiles <- quantile(randomDraw, 0:kk/kk)
  whichK <- cut(randomDraw, kQuantiles, include.lowest = TRUE)  # Divide randomDraw into kk equally-sized groups
  levels(whichK) <- LETTERS[1:kk]  # (Optionally) Give the levels handier names
  
  #returns the index vector
  return(whichK)
}



# build an index
kk <- 10 #n of cv folds
whichK <- build.whichK(kk, data)

# for loop on cv subsets
cv.result <- NULL
for(k in levels(whichK)){
  #append results of specific cv as a column
  cv.result <- rbind(cv.result, nb.k.errf(k, whichK, data, labels))
}


mean(cv.result)

