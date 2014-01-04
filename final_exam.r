library(e1071)

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

learn.svm <- function(x_in, y_in, gamma, cost, Q) {
	model <- svm(x_in, factor(y_in), type="C-classification", scale= FALSE, cost=cost, kernel = "polynomial", coef0=1, gamma, degree=Q)
}

exercise11 <- function() {
	train.data <- read.table("train.data")
	x <- cbind(train.data[,1], train.data[,2])
	y <- as.vector(train.data[,3])
	x <- cbind(x[,2]^2 - 2*x[,1] - 1, x[,1]^2 - 2*x[,2] + 1)
	
	plot(x[y == 1,], col="green", xlim=c(min(x[,1]), max(x[,1])), ylim=c(min(x[,2]), max(x[,2])))
	points(x[y != 1,], col="red")
}

exercise12 <- function() {
	train.data <- read.table("train.data")
	x <- cbind(train.data[,1], train.data[,2])
	y <- as.vector(train.data[,3])
	model <- learn.svm(x, y, gamma=1/length(y), cost=1e10, Q=2)
	print(model$tot.nSV)
}

learn.svm.radial <- function(x_in, y_in, gamma, cost) {
	model <- svm(x_in, factor(y_in), type="C-classification", scale= FALSE, cost=cost, kernel = "radial", gamma)
}

generate.training.data <- function(N) {
	x <- matrix(runif(2*N, -1, 1), N, 2)
	y <- sign(x[,2] - x[,1] + 0.25*sin(pi*x[,1]))
	list(x=x, y=y)
}

compute.error <- function(model, x, y) {
	
	y_p <- predict(model, x)
	error <- mean(y_p != y)
	
	list(error = error, numSV = model$tot.nSV)
}


exercise13 <- function(sim, num_points) {
	ein <- numeric(0)
	for (i in 1:sim) {
		train.data <- generate.training.data(num_points)
		model <- learn.svm.radial(train.data$x, train.data$y, 1.5, 1e10)
		y <- predict(model, train.data$x)
		ein[i] <- mean(y != train.data$y)
	}
	ein
}
	
learn.lloyd.old <- function(x_in, y_in, g, K, max_iter) {
	num_points <- dim(x_in)[1]
	lambda <- matrix(runif(2*K, -1, 1), K, 2)
	lambda.old <- matrix(0, K, 2)
	model.old <- numeric(0)
	model <- numeric(0)
	for (iter in 1:max_iter) {
		#Pseudo-Inverse
		m <- matrix(0, num_points, K)
		for (i in 1:K) {
			m[, i] <- exp(-g*apply((x_in - lambda[i, ])^2, 1, sum))
		}
		model <- linear.regression.with.regularization(m, y_in, 0)
		
		#Recalculate centers
		distance <- matrix(0, num_points, K)
		for (i in 1:K) {
			distance[, i] <- apply((x_in - lambda[i, ])^2, 1, sum)
		}
		min.distance <- apply(distance, 1, which.min)
		if (sum(1:K %in% min.distance) < K) {
			lambda <- matrix(runif(2*K, -1, 1), K, 2)
			print(iter)
			print(1:K %in% min.distance)
			print(min.distance)
		} else {
			for (i in 1:K) {
				if (sum(min.distance == i) > 1)
					lambda[i, ] <- apply(x_in[min.distance == i,], 2, mean)
				else
					lambda[i, ] <- x_in[min.distance == i]
			}
		}
	
		if (sum((lambda.old - lambda)^2) < 1e-3) {
			break;
		}
		
		lambda.old <- lambda
		model.old <- model
	}
	
	list(model = model, lambda = lambda, iter = iter)
}


learn.lloyd <- function(x_in, y_in, g, K, max_iter) {
	num_points <- dim(x_in)[1]
	centers <- kmeans(x$x, K, algorithm="Lloyd", iter.max=1000)
	m <- matrix(0, num_points, K)
	for (i in 1:K) {
		m[, i] <- exp(-g*apply((x_in - centers$centers[i, ])^2, 1, sum))
	}
	model <- linear.regression.with.regularization(m, y_in, 0)
	list(model = model, lambda = centers$centers, iter=1)
}
	
exercise14 <- function(sim, num_points, num_out_points) {
	eout <- matrix(0, sim, 2)
	K <- 12
	g <- 1.5
	iter <- numeric(0)
	max_iter <- 1000
	for (i in 1:sim) {
		train.data <- generate.training.data(num_points)
		model.svm <- learn.svm.radial(train.data$x, train.data$y, 1.5, 1e10)
		model.rbf <- learn.lloyd(train.data$x, train.data$y, 1.5, K, max_iter)
		
		test.data <- generate.training.data(num_out_points)
		y.svm <- predict(model.svm, test.data$x)
		m <- matrix(0, num_points, K)
		for (j in 1:K) {
			m[, j] <- exp(-g*apply((train.data$x - model.rbf$lambda[j, ])^2, 1, sum))
		}
		y.rbf <- evaluate.points(model.rbf$model, m)
		iter[i] <- model.rbf$iter
		if (iter[i] < max_iter) {
			eout[i,1] <- mean(y.svm != train.data$y)
			
		}
		eout[i,2] <- mean(y.rbf != train.data$y)
	}
	list(eout=eout, iter=iter)
}
	
exercise16 <- function(sim, num_points, num_out_points) {
	eout <- matrix(0, sim, 2)
	ein <- matrix(0, sim, 2)
	K <- 12
	g <- 1.5
	iter <- numeric(0)
	max_iter <- 1000
	for (i in 1:sim) {
		train.data <- generate.training.data(num_points)
		model.svm <- learn.lloyd(train.data$x, train.data$y, 1.5, 9, max_iter)
		model.rbf <- learn.lloyd(train.data$x, train.data$y, 1.5, 12, max_iter)
		m1 <- matrix(0, num_points, 9)
		for (j in 1:9) {
			m[, j] <- exp(-g*apply((train.data$x - model.svm$lambda[j, ])^2, 1, sum))
		}
		m2 <- matrix(0, num_points, 12)
		for (j in 1:12) {
			m[, j] <- exp(-g*apply((train.data$x - model.rbf$lambda[j, ])^2, 1, sum))
		}
		y.svm <- evaluate.points(model.svm$model, m1)
		y.rbf <- evaluate.points(model.rbf$model, m2)
		eout[i,1] <- mean(y.svm != train.data$y)
		eout[i,2] <- mean(y.rbf != train.data$y)
		
		
		test.data <- generate.training.data(num_out_points)
		
		m1 <- matrix(0, num_out_points, 9)
		for (j in 1:9) {
			m[, j] <- exp(-g*apply((test.data$x - model.svm$lambda[j, ])^2, 1, sum))
		}
		m2 <- matrix(0, num_out_points, 12)
		for (j in 1:12) {
			m[, j] <- exp(-g*apply((test.data$x - model.rbf$lambda[j, ])^2, 1, sum))
		}
		y.svm <- evaluate.points(model.svm$model, m1)
		y.rbf <- evaluate.points(model.rbf$model, m2)
		iter[i] <- model.rbf$iter
		if (iter[i] < max_iter) {
			eout[i,1] <- mean(y.svm != test.data$y)
			
		}
		eout[i,2] <- mean(y.rbf != test.data$y)
	}
	list(eout=eout, iter=iter)
}
	
	
	
	
	
	
	
	
	

