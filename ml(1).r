linear.regression <- function(x, y)
{
	m <- as.matrix(x)
	mt <- t(m)
	(solve(mt %*% m) %*% mt) %*% y
}


overfiting <- function()
{
	in_data <- read.table("in.txt")
	x1 <- in_data$V1
	x2 <- in_data$V2
	y <- in_data$V3
	
	n <- length(x1)
	m <- matrix( c(rep(1, n), x1, x2, x1^2, x2^2, x1*x2, abs(x1- x2), abs(x1 + x2)), n, 8)
	w <- linear.regression(m,y)
	Ein <- mean(y != sign(m%*%w))
	
	out_data <- read.table("out.txt")
	x1 <- out_data$V1
	x2 <- out_data$V2
	
	n <- length(x1)
	m <- matrix( c(rep(1, n), x1, x2, x1^2, x2^2, x1*x2, abs(x1- x2), abs(x1 + x2)), n, 8)
	y <- out_data$V3
	Eout <- mean(y != sign(m%*%w))
	
	print(paste("In of sample error:  ", Ein))
	print(paste("Out of sample error: ", Eout)) 
	
	
	# t <- seq(-1, 1, 0.05)
	# plot(t, -r[1] + -r[2]*t, type="l", col="red", ylim=c(-1, 1))
	# lines(t, -g[1]/g[3] + -g[2]/g[3]*t, type="l", col="green")
	# p <- x[y != 1, ]
	# p <- cbind(p[,2], p[,3])
	# points(p, col="red")
	# q <- x[y == 1, ]
	# q <- cbind(q[,2], q[,3])
	# points(q, col="green")


}