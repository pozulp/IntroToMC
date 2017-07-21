N <- 4 ** 2            # number of replications
x <- runif(N, -4, 4)   # randomly sample 8x1 rect centered at 0 by generating
y <- runif(N, 0, 1)    # N random variates (x,y) where x ~ U(-4,4), y ~ U(0,1)

fx <- exp(-(x*x))      # evaluate function to find position on curve 
inside <- y < fx       # if y < f(x) the sampled point is inside the curve

# scale inside/total by the total area to get the estimator for the integral
inside_ratio <- sum(inside) / length(inside)
total_area <- 8 
integral_estimator <- inside_ratio * total_area

# estimator^2 should converge to pi (recall our analytic solution was sqrt(pi))
pi_estimate <- integral_estimate ** 2
print(pi_estimate)
