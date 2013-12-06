f <- function(x)
{
	sin(pi*x)
}
calculaRecta <- function(x, y)
{
	m <- as.matrix(x)
	mt <- t(m)
	(solve(mt %*% m) %*% mt) %*% y
}

ejercicio4 <- function()
{
	N <- 100000
	x <- matrix(runif(2*N, -1, 1), N, 2)
	y <- f(x)
	m <- numeric(0)
	for (i in 1:N)
	{
		m <- c(m, calculaRecta(x[i, ], y[i, ]))
	}
	
	plot(f, c(-1,1))
	points(x[N,], y[N,], col="red")
	points(0, 0, col="blue")
	t <- seq(-1, 1, 0.05)
	result <- m[N]*t
	lines(t, result, col="green")

	salida <- m
}

ejercicio5 <- function()
{
	N <- 100000
	x <- matrix(runif(2*N, -1, 1), N, 2)
	y <- f(x)
	m <- numeric(0)
	for (i in 1:N)
	{
		m <- c(m, calculaRecta(x[i, ], y[i, ]))
	}
	
	pendiente <- mean(m)
	t <- seq(-1, 1, 0.01)
	y <- f(t)
	yApprox <- pendiente*t
	
	salida <- mean((y- yApprox)^2)
}



ejercicio6 <- function()
{
	N <- 100000
	x <- matrix(runif(2*N, -1, 1), N, 2)
	y <- f(x)
	m <- numeric(0)
	for (i in 1:N)
	{
		m <- c(m, calculaRecta(x[i, ], y[i, ]))
	}
	
	pendiente <- mean(m)
	t <- seq(-1, 1, 0.01)
	y <- pendiente*t
	error <- numeric(0)
	for (i in 1:N)
	{
		yApprox <- m[i]*t
		error <- c(error, mean((y - yApprox)^2))
	}
	
	error
}


calculaRectaMasPendiente <- function(x, y)
{
	m <- cbind(1, as.matrix(x))
	mt <- t(m)
	(solve(mt %*% m) %*% mt) %*% y
}

calculaParabola <- function(x, y)
{
	a <- (y[1] - y[2])/(x[1]^2 - x[2]^2)
	b <- y[1] - a*x[1]^2
	c(a,b)
}

ejercicio7 <- function(x,y)
{
	N <- 100000
	x <- matrix(runif(2*N, -1, 1), N, 2)
	y <- f(x)
	recta <- matrix(0, N, 2)
	parabola <- matrix(0, N, 2)
	
	for (i in 1:N)
	{
		recta[i,] <- calculaRectaMasPendiente(x[i, ], y[i, ])
		parabola[i,] <- calculaParabola(x[i, ], y[i, ])
	}
	
	
	t <- seq(-1, 1, 0.05)
	plot(f, t)
	points(x[N,], y[N,], col="red")
	points(0, 0)
	resultRecta <- recta[N,2]*t + recta[N,1]
	lines(t, resultRecta, col="green")
	resultParabola <- parabola[N,1]*t^2 + parabola[N,2]
	lines(t, resultParabola, col="blue")
	
	r <- apply(recta, 1, mean)
	para <- apply(parabola, 1, mean)
	y <- f(t)
	yRecta <- r[2]*t + r[1]
	yParabola <- para[1]*t^2 + para[2]
	salida <- c( mean((y- yRecta)^2), mean((y-yParabola)^2))

}