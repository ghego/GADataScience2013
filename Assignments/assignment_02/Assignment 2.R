library(class)
library(ggplot2)
data <- iris
labels <- data$Species
data$Species <- NULL
set.seed(1)

N <- nrow(data)

n <- 10 #n of cv folds (must be a divider of N)
max.k <- 50 #max number of nearest neighbors

#Function to run one iteration of knn for k in [1:max.k]
do.knn.subset <- function(max.k, i, train.data, test.data, train.labels, test.labels){
  #data frame that will contain the result
  err.rates <- data.frame()
  
  #for loop on k
  for(k in 1:max.k){
    knn.fit <- knn(train = train.data,
                   test = test.data,
                   cl = train.labels,
                   k = k
    )
    
    # calculate error
    this.err <- sum(test.labels != knn.fit)/length(test.labels)
    
    # append error to results column
    err.rates <- rbind(err.rates, this.err)
  }
  
  #return the column of results
  return(err.rates)
}


# function to do the cross validation on n subsets for a given range of k
do.cv <- function(n, max.k, data, labels){
  #helper factor that we will use to split the data into n subsets
  indexc <- as.factor(c(rep(1:n, N/n)))
  data <- cbind(data, indexc)
  labels <- cbind(labels, indexc)
  
  #results data.frame
  cv.result <- data.frame(1:max.k)
  colnames(cv.result) <- "knn"

  # for loop on cv subsets
  for(i in 1:n){
    
    #assign the training and test subsets
  	train.data <- subset(data, indexc != i)[,1:4]
  	test.data <- subset(data, indexc == i)[,1:4]
  	train.labels <- subset(labels, indexc != i)[,1]
  	test.labels <- subset(labels, indexc == i)[,1]
    
    #run cv using function above
    err.rates.temp <- do.knn.subset(max.k, i, train.data, test.data, train.labels, test.labels)
    
    #append results of specific cv as a column
    cv.result <- cbind(cv.result, err.rates.temp)
  }
  return(cv.result)
}


#cv.err.rates is a table of max.k rows and n columns
cv.err.rates <- do.cv(n, max.k, data, labels)

#calculate mean of cv folds for each k
results <- data.frame(1:max.k, apply(cv.err.rates[,2:length(cv.err.rates)], 1, mean))

#adjust names
names(results) <- c("k", "cv.err.rate")

#plot the rsults
title <- paste('knn cross-validation results (n subsets = ', n, ')', sep='')
results.plot <- ggplot(results, aes(x=k, y=cv.err.rate)) + geom_point() + geom_line()
results.plot <- results.plot + ggtitle(title)
print(results.plot)

results.plot <- ggplot(results, aes(x=k, y=cv.err.rate)) + geom_smooth()

print(results.plot)
  
  
