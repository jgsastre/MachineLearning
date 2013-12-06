
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
	sign(x %*% r)
}

calculaG <- function(x, y) {
	xPrima <- t(x)
	xDaga <- solve(xPrima %*% x) %*% xPrima
	xDaga %*% y
}

ejercicio5 <- function(Puntos, Simulaciones) {
	salida <- rep(0, Simulaciones)
	for (i in 1:Simulaciones) {
		x <- sorteaPuntos(Puntos)
		r <- sorteaRecta()
		y <- evaluaPuntos(x, r)
		g <- calculaG(x, y)
		yp <- evaluaPuntos(x, g)
		salida[i] <- mean(yp != y)
	}
	
	return(salida)
}
		
ejercicio6 <- function(PuntosIn, PuntosOut, Simulaciones) {
	salida <- rep(0, Simulaciones)
	for (i in 1:Simulaciones) {
		x <- sorteaPuntos(PuntosIn)
		r <- sorteaRecta()
		y <- evaluaPuntos(x, r)
		g <- calculaG(x, y)
		xOut <- sorteaPuntos(PuntosOut)
		yOut <- evaluaPuntos(xOut, r)
		ypOut <- evaluaPuntos(xOut, g)
		
		salida[i] <- mean(ypOut != yOut)
	}
	
	return(salida)
}		

perceptron <- function(y, f, g, x) {
	newG <- g
	indices <- 1:length(y)
	for (n in 1:100) {
		yp = evaluaPuntos(x, newG)
		errores <- y != yp
		L = sum(errores)
		if (L == 0) {
			break;
		}
		indiceErrores <- indices[errores]
		indiceAleatorio <- indiceErrores[sample(1:L, 1)]
		p <- x[indiceAleatorio, ]
		newG <- newG + evaluaPuntos(p,f)*p
	}
	
	return(list("n"= n, "g" = newG))
}

ejercicio7 <- function(PuntosIn, Simulaciones) {
	salida <- matrix(0, Simulaciones, 2)
	for (i in 1:Simulaciones) {
		x <- sorteaPuntos(PuntosIn)
		r <- sorteaRecta()
		y <- evaluaPuntos(x, r)
		g <- calculaG(x, y)
		z <- perceptron(y, r, g, x)
		
		yp <- evaluaPuntos(x, z$g)
		print(paste("n: ", z$n))
		print(paste("mean: ", mean(yp != y)))
		salida[i, ] <- c(z$n, mean(yp != y))
	}
	
	return(salida)
}		
		
		
		