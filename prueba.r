#!/usr/bin/env Rscript

# Number of Runs of the Experiment.
R = 1

# Sample Size.
N = 100

# X part of set of samples.
X = NA

# Y part of set of samples.
Y = NA

#
# Target Function and its Weight Vector.
# a, b are the Random Points which define the Function.
#
f = NA; wf = NA; a = NA; b = NA

# Final Hypothesis obtained via Logistic Regression.
g = NA; wg = NA

#
# Per Experiment:
# IN-SAMPLE Cross-Entropy Error = Ex<ln(1 + exp(-y w.x))>
#
Ein = vector(length = R)

#
# Per Experiment:
# OUT-OF-SAMPLE Cross-Entropy Error = Ex<ln(1 + exp(-y w.x))>
#
Eout = vector(length = R)

#
# Per Experiment:
# Iterations taken by SGD for convergence.
#
Iter = vector(length = R)

dotp = function (u, v) t(u) %*% v
norm = function (u) sqrt(sum(u^2))

random_x = function (void) c(1, runif(2, -1, 1))

random_f = function () {
    a <<- random_x()
    b <<- random_x()
    
    wf[1] <<- (a[2] * b[3]) - (b[2] * a[3])
    wf[2] <<- a[3] - b[3]
    wf[3] <<- b[2] - a[2]

    function (x) { sign(dotp(wf, x)) == 1 }
}

get_y  = function (x) { if (runif(1) < f(x)) +1 else -1 }

gradE = function (x, y, w) {
    c = -y / (1 + exp(y * dotp(w, x)))
    c * x
}

sgd = function () {

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

# Cross Entropy Error.
E = function (x, y, w) log(1 + exp(-y * dotp(w, x)))

ein = function () {
    error = 0

    for (i in 1:N) {
        x = X[i, ]
        y = Y[i]
        error = error + E(x, y, wg)
    }

    error / N
}

eout = function () {
    T = 1000
    Xout = t(sapply(1:T, random_x))
    Yout = apply(Xout, 1, get_y)

    error = 0

    for (i in 1:T) {
        x = Xout[i, ]
        y = Yout[i]
        error = error + E(x, y, wg)
    }

    error / T
}

for (i in 1:R) {

    # Generate Data Set D.
    f = random_f()
    X = t(sapply(1:N, random_x))
    Y = apply(X, 1, get_y)

    out = sgd()
    wg  = out$w

    Iter[i] = out$iter
    Eout[i] = eout()
    Ein[i]  = ein()
}

t <- seq(-1, 1, 0.05)
plot(t, -wf[1]/wf[3] + -wf[2]/wf[3]*t, type="l", col="red", ylim=c(-1, 1))
lines(t, -wg[1]/wg[3] + -wg[2]/wg[3]*t, type="l", col="green")
p <- X[Y != 1, ]
p <- cbind(p[,2], p[,3])
points(p, col="red")
q <- X[Y == 1, ]
q <- cbind(q[,2], q[,3])
points(q, col="green")


print(paste("Number of Runs      =", R))
print(paste("Sample Size         =", N))
print(paste("In-Sample Error     =", mean(Ein)))
print(paste("Out-of-Sample Error =", mean(Eout)))
print(paste("Iterations for SGD  =", mean(Iter)))