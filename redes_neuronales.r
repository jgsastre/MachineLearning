

e <- function(z) {
	u <- z[1]
	v <- z[2]
	(u*exp(v) - 2*v*exp(-u))^2
}

eprima <- function(z) {
	u <- z[1]
	v <- z[2]
	2*(u*exp(v)-2*v*exp(-u))*c(exp(v) + 2*v*exp(-u), u*exp(v) - 2*exp(-u))
}

minimiza <- function() {
	eta <- 0.1
	z <- c(1,1)
	n <- 0
	for (i in 1:20) {
		delta_z <- eprima(z)
		z <- z - eta*delta_z
		n <- n + 1
		if (abs(e(z)) < 1e-14) {
			break;
		}
	}
	
	print(z)
	print(n)
}


minimiza_cd <- function() {
	eta <- 0.1
	z <- c(1,1)
	for (i in 1:15) {
		delta_z <- c(eprima(z)[1], 0)
		print(delta_z)
		z <- z - eta*delta_z
		delta_z <- c(0, eprima(z)[2])
		print(delta_z)
		z <- z - eta*delta_z
	}
	
	print(e(z))
}

sorteaRecta <- function() {
	x <- matrix(runif(4)*2 - 1, 2, 2)
	p1 <- x[1,]
	p2 <- x[2,]
	m <- (p2[2] - p1[2])/(p2[1] - p1[1])
	a <- p1[2] - p1[1]*m
	c(-a, -m, 1)
}

sorteaPuntos <- function(N) {
	x <- cbind(1, matrix(runif(2*N, -1, 1), N, 2))
}

evaluaPuntos <- function(x, r) {
	y <- sign(x %*% r)
}

calculaG <- function(x, y) {
	eta <- 0.01
	w <- c(0,0,0)
	for (epoch in 1:10000) {
		N <- nrow(X)
		new_w <- w
		for (i in sample(1:N)) {
			x_i <- X[i, ]
			y_i <- Y[i]
			delta <- -(x_i*y_i)/(1+exp(y_i*  t(new_w) %*%x_i))
			new_w <- new_w - eta*delta
		}
		
		if ( sqrt(sum((new_w - w)^2)) < 0.01) {
			break;
		}
		
		w <- new_w
	}
	
	list(w= new_w, iter = epoch)
}


LogisticRegression <- function(Puntos, Simulaciones) {
	
	lista_Eout <- numeric(0)
	lista_epoch <- numeric(0)
	for (i in 1:Simulaciones) {
		x <- sorteaPuntos(Puntos)
		r <- sorteaRecta()
		y <- evaluaPuntos(x, r)
		calculado <- calculaG(x, y)
		g <- calculado$w
		x_new <- sorteaPuntos(1000)
		y_eout <- evaluaPuntos(x_new, r)
		lista_Eout <- c(lista_Eout, mean(log(1+exp(-y_eout* x_new%*%g))))
		lista_epoch <- c(lista_epoch, calculado$iter)
	}
	
	t <- seq(-1, 1, 0.05)
	plot(t, -r[1] + -r[2]*t, type="l", col="red", ylim=c(-1, 1))
	lines(t, -g[1]/g[3] + -g[2]/g[3]*t, type="l", col="green")
	p <- x[y != 1, ]
	p <- cbind(p[,2], p[,3])
	points(p, col="red")
	q <- x[y == 1, ]
	q <- cbind(q[,2], q[,3])
	points(q, col="green")
	
	salida <- data.frame(Eout = lista_Eout, epoch = lista_epoch)
}



