linear.regression.with.regularization <- function(x, y, lambda)
{
	m <- as.matrix(cbind(1,x))
	mt <- t(m)
	(solve(mt %*% m + lambda*diag(dim(m)[2])) %*% mt) %*% y
}

evaluate.points <- function(r, x) {	
	y <- sign(cbind(1,x) %*% r)
}

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

compare.classifiers <- function(train.data, test.data, d1, d2, learn, predict) {
	ein <- numeric(0)
	eout <- numeric(0)
	for(i in 1:length(d1)) {
		train.input <- build.data(train.data, d1[i], d2[i])
		model <- learn(train.input$x, train.input$y)
		train.result <- predict(model, train.input$x)
		ein[i] <- mean(train.result != train.input$y)

		test.input <- build.data(test.data, d1[i], d2[i])
		test.result <- predict(model, test.input$x)
		eout[i] <- mean(test.result != test.input$y)

		print(paste(d1[i], " against ", d2[i], " Ein: ", ein[i], " Eout: ", eout[i]))
	}
	print(paste("Min value of Ein for ", d1[which.min(ein)], " against ", d2[which.min(ein)], 
		" with Ein: ", ein[which.min(ein)]))
	print(paste("Max value of Ein for ", d1[which.max(ein)], " against ", d2[which.max(ein)], 
		" with Ein: ", ein[which.max(ein)]))
	print(paste("Min value of Eout for ", d1[which.min(eout)], " against ", d2[which.min(eout)], 
		" with Eout: ", eout[which.min(eout)]))
	print(paste("Max value of Eout for ", d1[which.max(eout)], " against ", d2[which.max(eout)], 
		" with Ein: ", eout[which.max(eout)]))

	list(ein=ein, eout=eout)
}


compare.classifiers.and.learning.methods <- function(train.data, test.data, d1, d2, learn, predict) {
	ein <- matrix(0, length(d1), length(learn))
	eout <- matrix(0, length(d1), length(learn))
	for(i in 1:length(d1)) {
		for (j in 1:length(learn)) {
			train.input <- build.data(train.data, d1[i], d2[i])
			model <- learn[[j]](train.input$x, train.input$y)
			train.result <- predict[[j]](model, train.input$x)
			ein[i,j] <- mean(train.result != train.input$y)

			test.input <- build.data(test.data, d1[i], d2[i])
			test.result <- predict[[j]](model, test.input$x)
			eout[i,j] <- mean(test.result != test.input$y)
		}
	}

	list(ein=ein, eout=eout)
}

exercise7 <- function() {
	d1 <- c(5, 6, 7, 8, 9)
	d2 <- rep(-1, length(d1))
	learn.method <- function(x,y) {
		linear.regression.with.regularization(x,y,1)
	}
	compare.classifiers(read.train.file(), read.test.file(), d1, d2, learn.method, evaluate.points)	
}

exercise8 <- function() {
	d1 <- 0:4
	d2 <- rep(-1, length(d1))
	learn.method <- function(x,y) {
		x <- cbind(x, x[,1]*x[,2], x[,1]^2, x[,2]^2)
		linear.regression.with.regularization(x,y,1)
	}
	evaluate.method <- function(model, x) {
		x <- cbind(x, x[,1]*x[,2], x[,1]^2, x[,2]^2)
		evaluate.points(model, x)
	}
	compare.classifiers.eout(read.train.file(), read.test.file(), d1, d2, learn.method, evaluate.method)	
}



exercise9 <- function() {
	d1 <- 0:9
	d2 <- rep(-1, length(d1))
	learn.method <- function(x,y) {
		linear.regression.with.regularization(x,y,1)
	}
	learn.method.with.transformation <- function(x,y) {
		x <- cbind(x, x[,1]*x[,2], x[,1]^2, x[,2]^2)
		linear.regression.with.regularization(x,y,1)
	}
	evaluate.method.with.transformation <- function(model, x) {
		x <- cbind(x, x[,1]*x[,2], x[,1]^2, x[,2]^2)
		evaluate.points(model, x)
	}
	
	learn.methods <- c(learn.method.with.transformation, learn.method)
	evaluate.methods <- c(evaluate.method.with.transformation, evaluate.points)
	compare.classifiers.and.learning.methods(read.train.file(), read.test.file(), d1, d2, learn.methods, evaluate.methods)	
}


exercise10 <- function() {
	d1 <- 1
	d2 <- 5
	
	learn.method.with.transformation.and.lesser.lambda <- function(x,y) {
		x <- cbind(x, x[,1]*x[,2], x[,1]^2, x[,2]^2)
		linear.regression.with.regularization(x,y,0.01)
	}
	learn.method.with.transformation.and.greater.lambda <- function(x,y) {
		x <- cbind(x, x[,1]*x[,2], x[,1]^2, x[,2]^2)
		linear.regression.with.regularization(x,y,1)
	}
	evaluate.method.with.transformation <- function(model, x) {
		x <- cbind(x, x[,1]*x[,2], x[,1]^2, x[,2]^2)
		evaluate.points(model, x)
	}

	learn.methods <- c(learn.method.with.transformation.and.lesser.lambda, learn.method.with.transformation.and.greater.lambda)
	evaluate.methods <- c(evaluate.method.with.transformation, evaluate.method.with.transformation, evaluate.method.with.transformation)
	compare.classifiers.and.learning.methods(read.train.file(), read.test.file(), d1, d2, learn.methods, evaluate.methods)	
}



