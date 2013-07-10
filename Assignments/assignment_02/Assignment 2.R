library(class)
library(ggplot2)
data <- iris
labels <- data$Species
data$Species <- NULL
set.seed(1)
train.pct <- 0.7
N <- nrow(data)
train.index <- sample(1:N, train.pct * N)

train.data <- data[train.index, ]
test.data <- data[-train.index, ]

train.labels <- as.factor(as.matrix(labels)[train.index,])
test.labels <- as.factor(as.matrix(labels)[-train.index, ])

err.rates <- data.frame()
max.k <- 100


for (k in 1:max.k)
{
	knn.fit <- knn(train = train.data,
	test = test.data,
	cl = train.labels,
	k = k
	)
	cat('\n', 'k = ', k, ', train.pct = ',train.pct, '\n', sep ='')
	print(table(test.labels, knn.fit))

	this.err <- sum(test.labels != knn.fit)/length(test.labels)
	err.rates <- rbind(err.rates, this.err)
}

results <- data.frame(1:max.k, err.rates)
names(results) <- c('k', 'err.rate')

title <- paste('knn results (train.pct = ', train.pct, ')', sep='')
results.plot <- ggplot(results, aes(x=k, y=err.rate)) + geom_point() + geom_line()
results.plot <- results.plot + ggtitle(title)
print(results.plot)

results.plot <- ggplot(results, aes(x=k, y=err.rate)) + geom_smooth()

print(results.plot)