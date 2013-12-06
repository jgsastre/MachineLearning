library(e1071)

read.data.file <- function(f) {
	d <- read.table(f)
	x <- matrix(c(d$V2, d$V3), length(d$V2), 2)
	list(digit=d$V1,  p = x)
}

read.train.file <- function() {
	read.data.file("features.train")
}

read.test.file <- function() {
	read.data.file("features.test")
}

build.data <- function(raw.data, digit1, digit2) {
	index <- raw.data$digit == digit1
	x_1 <- raw.data$p[index, ]
	y_1 <- rep(1, sum(index))
	x_2 <- numeric(0)
	y_2 <- numeric(0)
	if (digit2 == -1) {
		x_2 <- raw.data$p[!index, ]
		y_2 <- rep(-1, sum(!index))
	} else {
		index <- raw.data$digit == digit2
		x_2 <- raw.data$p[index, ]
		y_2 <- rep(-1, sum(index))
	}
	
	list( x = rbind(x_1, x_2), y=c(y_1, y_2))
}

learn <- function(x_in, y_in, cost, Q) {
	model <- svm(x_in, factor(y_in), type="C-classification", scale= FALSE, cost=cost, kernel = "polynomial", coef0=1, gamma=1, degree=Q)
}

learn.cross.validation <- function(x_in, y_in, cost, Q) {
	model <- svm(x_in, factor(y_in), type="C-classification", scale= FALSE, cost=cost, kernel = "polynomial", coef0=1, gamma=1, degree=Q, cross=10)
}

learn.radial <- function(x_in, y_in, cost, Q) {
	model <- svm(x_in, factor(y_in), type="C-classification", scale= FALSE, cost=cost, kernel = "radial", gamma=1)
}

compute.error <- function(model, x, y) {
	
	y_p <- predict(model, x)
	error <- mean(y_p != y)
	
	list(error = error, numSV = model$tot.nSV)
}

compare.classifiers <- function(train.data, d1, d2, cost, Q) {
	ein <- numeric(0)
	numSV <- numeric(0)
	for(i in 1:length(d1)) {
		train.input <- build.data(train.data, d1[i], d2[i])
		model <- learn(train.input$x, train.input$y, cost, Q)
		result <- compute.error(model, train.input$x, train.input$y)
		ein[i] <- result$error
		numSV[i] <- result$numSV
		print(paste("Ein for ", d1[i], " against ", d2[i], ": ", ein[i], " and support vectors: ", result$numSV))
	}
	print(paste("Min value of Ein for ", d1[which.min(ein)], " against ", d2[which.min(ein)], " with Ein: ", ein[which.min(ein)], " and numSV: ", numSV[which.min(ein)]  ))
	print(paste("Max value of Ein for ", d1[which.max(ein)], " against ", d2[which.max(ein)], " with Ein: ", ein[which.max(ein)], " and numSV: ", numSV[which.max(ein)]  ))
	
	list(ein=ein, numSV = numSV)
}


compare.costs <- function(train.data, d1, d2, cost, Q) {
	eout <- numeric(0)
	ein <- numeric(0)
	numSV <- numeric(0)
	for(i in 1:length(cost)) {
		train.input <- build.data(train.data, d1, d2)
		model <- learn(train.input$x, train.input$y, cost[i], Q)
		result <- compute.error(model, train.input$x, train.input$y)
		ein[i] <- result$error
		numSV[i] <- result$numSV
		
		test.input <- build.data(read.test.file(), d1, d2)
		result <- compute.error(model, test.input$x, test.input$y)
		eout[i] <- result$error
		
		print(paste("For ", d1, " against ", d2, " with cost ", cost[i], " Ein: ", ein[i], 
			" and Eout:", eout[i], " and support vectors: ", result$numSV))
	}
	print(paste("Result analysis for ", d1, " against ", d2))
	print(paste("Min value of Ein for cost: ", cost[which.min(ein)], " with Ein: ", ein[which.min(ein)], " and numSV: ", numSV[which.min(ein)]  ))
	print(paste("Max value of Ein for cost: ", cost[which.max(ein)], " with Ein: ", ein[which.max(ein)], " and numSV: ", numSV[which.max(ein)]  ))
	print(paste("Min value of Eout for cost: ", cost[which.min(eout)], " with Ein: ", eout[which.min(eout)]))
	print(paste("Max value of Eout for cost: ", cost[which.max(eout)], " with Ein: ", eout[which.max(eout)])) 
	
	list(ein=ein, numSV = numSV)
}

compare.costs.and.Q <- function(train.data, d1, d2, cost, Q) {

	for(i in 1:length(cost)) {
		eout <- numeric(0)
		ein <- numeric(0)
		numSV <- numeric(0)
		for (j in 1:length(Q)) {
			train.input <- build.data(train.data, d1, d2)
			model <- learn(train.input$x, train.input$y, cost[i], Q[j])
			result <- compute.error(model, train.input$x, train.input$y)
			ein[j] <- result$error
			numSV[j] <- result$numSV
			
			test.input <- build.data(read.test.file(), d1, d2)
			result <- compute.error(model, test.input$x, test.input$y)
			eout[j] <- result$error
			
			print(paste("For ", d1, " against ", d2, " with cost ", cost[i], " and Q: ", Q[j], 
			" Ein: ", ein[j], " and Eout:", eout[j], " and support vectors: ", result$numSV))
		}	
		print(paste("Result analysis for ", d1, " against ", d2, " with cost: ", cost[i]))
		print(paste("Min value of Ein for Q: ", Q[which.min(ein)], " with Ein: ", ein[which.min(ein)], " and numSV: ", numSV[which.min(ein)]  ))
		print(paste("Max value of Ein for Q: ", Q[which.max(ein)], " with Ein: ", ein[which.max(ein)], " and numSV: ", numSV[which.max(ein)]  ))
		print(paste("Min value of Eout for Q: ", Q[which.min(eout)], " with Ein: ", eout[which.min(eout)]))
		print(paste("Max value of Eout for Q: ", Q[which.max(eout)], " with Ein: ", eout[which.max(eout)])) 
	}
}


exercise2 <- function() {
	d1 <- c(0, 2, 4, 6, 8)
	d2 <- rep(-1, length(d1))
	Q <- 2
	cost <- 0.01
	train.data <- read.train.file()
	compare.classifiers(train.data, d1, d2, cost, Q)	
}

exercise3 <- function() {
	d1 <- c(1, 3, 5, 7, 9)
	d2 <- rep(-1, length(d1))
	Q <- 2
	cost <- 0.01
	train.data <- read.train.file()
	compare.classifiers(train.data, d1, d2, cost, Q)	
}

exercise5 <- function() {
	d1 <- 1
	d2 <- 5
	Q <- 2
	cost <- c(0.001, 0.01, 0.1, 1)
	train.data <- read.train.file()
	compare.costs(train.data, d1, d2, cost, Q)	
}

exercise6 <- function() {
	d1 <- 1
	d2 <- 5
	Q <- c(2, 5)
	cost <- c(0.0001, 0.001, 0.01, 1)
	train.data <- read.train.file()
	compare.costs.and.Q(train.data, d1, d2, cost, Q)	
}
exercise7 <- function() {
	simulations <- 100
	costs <- c(1, 0.1, 0.01, 0.001, 0.00001)
	train.data <- read.train.file()
	train.input <- build.data(train.data, 1, 5)
	
	min_index <- numeric(0)
	for( sim in 1:simulations) {
		accuracy <- numeric(0)
		for (i in 1:length(costs)) {
			result <- learn.cross.validation(train.input$x, train.input$y, costs[i], Q=2)
			accuracy[i] <- result$tot.accuracy
		}

		min_index[sim] <- which.max(accuracy)
	}

	print(min_index)
	for (i in 1:length(costs))
	{
		print(sum(min_index == i))
	}
	
	return(min_index)
}



exercise8 <- function() {
	simulations <- 100
	costs <- 0.001
	train.data <- read.train.file()
	train.input <- build.data(train.data, 1, 5)
	
	accuracy <- numeric(0)
	for( sim in 1:simulations) {
		result <- learn.cross.validation(train.input$x, train.input$y, costs, Q=2)
		accuracy[sim] <- (100-result$tot.accuracy)/100
	}
	
	mean(accuracy)
}

compare.radial.costs <- function(train.data, d1, d2, cost, Q) {
	eout <- numeric(0)
	ein <- numeric(0)
	numSV <- numeric(0)
	for(i in 1:length(cost)) {
		train.input <- build.data(train.data, d1, d2)
		model <- learn.radial(train.input$x, train.input$y, cost[i], Q)
		result <- compute.error(model, train.input$x, train.input$y)
		ein[i] <- result$error
		numSV[i] <- result$numSV
		
		test.input <- build.data(read.test.file(), d1, d2)
		result <- compute.error(model, test.input$x, test.input$y)
		eout[i] <- result$error
		
		print(paste("For ", d1, " against ", d2, " with cost ", cost[i], " Ein: ", ein[i], 
			" and Eout:", eout[i], " and support vectors: ", result$numSV))
	}
	print(paste("Result analysis for ", d1, " against ", d2))
	print(paste("Min value of Ein for cost: ", cost[which.min(ein)], " with Ein: ", ein[which.min(ein)], " and numSV: ", numSV[which.min(ein)]  ))
	print(paste("Max value of Ein for cost: ", cost[which.max(ein)], " with Ein: ", ein[which.max(ein)], " and numSV: ", numSV[which.max(ein)]  ))
	print(paste("Min value of Eout for cost: ", cost[which.min(eout)], " with Ein: ", eout[which.min(eout)]))
	print(paste("Max value of Eout for cost: ", cost[which.max(eout)], " with Ein: ", eout[which.max(eout)])) 
	
	list(ein=ein, numSV = numSV)
}



exercise9 <- function() {
	d1 <- 1
	d2 <- 5
	Q <- 2
	cost <- c(0.01, 1, 100, 1e4, 1e6)
	train.data <- read.train.file()
	compare.radial.costs(train.data, d1, d2, cost, Q)	
}




