N <- 10 ** 5                # number of replications
theta <- 10                 # from problem description

x1 <- runif(N, 0, theta)    # randomly sample 10x10 rect by generating 
x2 <- runif(N, 0, theta)    # N random samples (x1, x2) ~ (U(0,10),U(0,10))

xmat <- matrix(c(x1, x2), ncol = 2) 
xbar <- apply(xmat, 1, mean)# Create Nx2 matrix and use apply() to compute 
xmax <- apply(xmat, 1, max) # entry-by-entry {mean,min,max}(x1, x2)
xmin <- apply(xmat, 1, min) # where each pair (x1, x2) is one sample

est1 <- 2 * xbar            # Use sample statistics to compute the
est2 <- 3 * xmax / 2        # three point estimators given in the 
est3 <- 3 * xmin            # problem description

diff1 = abs(est1 - theta)   # Winning estimator is the one which is closest
diff2 = abs(est2 - theta)   # to the parameter value. Create Nx3 matrix and
diff3 = abs(est3 - theta)   # use apply() to get winner for each sample.

diffmat <- matrix(c(diff1, diff2, diff3), ncol = 3)
closest <- apply(diffmat, 1, which.min) 

p1hat <- sum(closest == 1) / length(closest) # Quotient wins/total is the MC
p2hat <- sum(closest == 2) / length(closest) # estimator for the prob that 
p3hat <- sum(closest == 3) / length(closest) # estimator is closest to theta
