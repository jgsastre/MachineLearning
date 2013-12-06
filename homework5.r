ejercicio1 <- function() {
N <- c(4, 4.2, 4.4, 4.6, 4.8)*100000

epsilon <- function(sigma, dvc, N) {
	sqrt(8/N*log(4*(2*N)^dvc/sigma))
}

bound <- function(epsilon, dvc, N) {
	4*(2*N)^dvc*exp(-1/8*epsilon^2*N)
}

salida <- data.frame(N = N, epsilon = epsilon(0.05, 10, N), 
	bound = bound(0.05, 10, N))
salida

}