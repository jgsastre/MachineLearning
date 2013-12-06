linear.regression <- function(x, y) {
	xPrima <- t(x)
	xDaga <- solve(xPrima %*% x) %*% xPrima
	xDaga %*% y
}

random.line <- function() {
	x <- matrix(runif(4)*2 - 1, 2, 2)
	p1 <- x[1,]
	p2 <- x[2,]
	m <- (p2[2] - p1[2])/(p2[1] - p1[1])
	a <- p1[2] - p1[1]*m
	c(-a, -m, 1)
}

random.points <- function(N) {
	x <- cbind(1, matrix(runif(2*N, -1, 1), N, 2))
}

evaluate.points <- function(x, r) {	
	y <- sign(x %*% r)
}

calculate.error <- function(x,y,w) {
	mean(evaluate.points(x,w) != y)
}

read.data.file <- function(f) {
	d <- read.table(f)
	x <- matrix(c(d$V1, d$V2), length(d$V1), 2)
	list(x=x, y = d$V3)
}

read.in.file <- function() {
	read.data.file("in.dta")
}

read.out.file <- function() {
	read.data.file("out.dta")
}

transformation <- function(x) {
	cbind(1, x[,1], x[,2], x[,1]^2, x[,2]^2, x[,1]*x[,2], abs(x[,1] - x[,2]), abs(x[,1] + x[,2]))
}

train.validate.select <- function(trainingIndex) {
	inData <- read.in.file();
	k_index <- c(3,4,5,6,7) + 1
	x_learn <- inData$x[trainingIndex, ]
	y_learn <- inData$y[trainingIndex]
	x_validate <- inData$x[!trainingIndex, ]
	y_validate <- inData$y[!trainingIndex]
	
	outData <- read.out.file();
	
	validation_error <- numeric(0)
	Eout <- numeric(0)
	x_learn_transformed <- transformation(x_learn)
	x_validate_transformed <- transformation(x_validate)
	x_out_transformed <- transformation(outData$x)
	i <- 1
	for (k in k_index) {
		w <- linear.regression(x_learn_transformed[, 1:k], y_learn)
		validation_error[i] <- calculate.error(x_validate_transformed[, 1:k], y_validate, w)
		Eout[i] <- calculate.error(x_out_transformed[, 1:k], outData$y, w)
		
		print(paste("Transformation ", i, ": validation error: ", validation_error[i], " Eout: ", Eout[i]))
		i <- i + 1
	}
	
	print("#########################################################################")
	print(paste("Minimum method for validation error: ", which.min(validation_error), " with an validation error: ", min(validation_error)))
	print(paste("Minimum method for out of sample error: ", which.min(Eout), " with an out of sample error: ", min(Eout)))
	
	list(Eval = validation_error, Eout=Eout)
}

exercise1 <- function() {
	inData <- read.in.file();
	trainingIndex <- seq(1, dim(inData$x)[1]) <= 25
	train.validate.select(trainingIndex)
}

exercise3 <- function() {
	inData <- read.in.file();
	trainingIndex <- seq(1, dim(inData$x)[1]) <= 25
	train.validate.select(!trainingIndex)
}

exercise7 <- function() {
	x <- c(sqrt(sqrt(3)+4), sqrt(sqrt(3)-1), sqrt(9 + 4*sqrt(6)), sqrt(9-sqrt(6)))
	
	distance <- function(x) {
		4/(x+1)^2 + 4/(x-1)^2 + 1
	}
	
	sapply(x, distance)
}

resolve.line.pla <- function(x, y) {
	newG <- rep(0, 3)
	for (i in 1:100) {
		print(newG)
		yp = evaluate.points(x, newG)
		if (sum(y != yp) == 0) {
			break
		}
		
		diff_index <- y != yp
		print(diff_index)
		index <- (1:length(y))[diff_index][sample(1:length(diff_index), 1)]
		newG <- newG + y[index]*x[index, ]
	}
	
	return(newG)
}

comparison.pla.vsm <- function(num_sim, in_points, out_points) {

	eout <- numeric(0)
	for (i in 1:num_sim) {
		f <- random.line()
		x_in <- random.points(in_points)
		y_in <- evaluate.points(x_in, f)
		g <- resolve.line.pla(x_in, y_in)

		x_out <- random.points(out_points)
		y_out <- evaluate.points(x_out, g)
		new_y <- evaluate.points(x_out, f)
		
		eout[i] <- mean(new_y != y_out)
	}
	
	print(g)
	t <- seq(-1, 1, 0.05)
	plot(t, -f[1] + -f[2]*t, type="l", col="red", ylim=c(-1, 1))
	lines(t, -g[1]/g[3] + -g[2]/g[3]*t, type="l", col="green")
	p <- x_in[y_in != 1, ]
	p <- cbind(p[,2], p[,3])
	points(p, col="red")
	q <- x_in[y_in == 1, ]
	q <- cbind(q[,2], q[,3])
	points(q, col="green")
}


exercise8 <- function() {
	comparison.pla.vsm(1, 10, 10)
}




