#!/bin/Rscript
# normal.R

fsf <- 2 # font size factor
dim <- 8 # dimension in inches of graphics region
darkgreen <- rgb(0, 153/256, 51/256)
darkred <- rgb(204/256, 0, 0)
endlogN <- 12
plot_convergence <- TRUE
plot_walltime <- TRUE
marker <- 20

f <- function(x) {
    exp(-(x*x))
}

# returns 2-tuple containing
# (pi_estimate, walltime)
mcint <- function(logN, fname = "normal") {
    base <- 4
    plot.new()
    plotname <- sprintf("%s%d.eps", fname, logN)
    postscript(plotname, width = dim, height = dim, horizontal = FALSE)
    n <- base ** logN

    # Perform MC integration
    # ----------------------
    starttime <- proc.time()
    x <- runif(n, -4, 4)
    y <- runif(n, 0, 1)

    hx <- f(x)
    inside <- y < hx 

    total_area <- 8
    integral_estimate <- ( sum(inside) / length(inside) ) * total_area
    pi_estimate <- integral_estimate ** 2
    time_to_compute_pi_estimate <- proc.time() - starttime
    walltime <- time_to_compute_pi_estimate[3] # 1:user, 2:sys, 3:elapsed

    print(sprintf("%d ** %2d = %7d, %f, %f", 
                  base, logN, n, pi_estimate, walltime))

    # Plot randomly-sampled (x,y) points
    # on top of a normal pdf f(x) = e^-x2
    # -----------------------------------
    integral_estimate_str <- sprintf("%1.4f", integral_estimate)
    pi_estimate_str <- sprintf("%1.4f", pi_estimate)
    n_str <- sprintf("4^%d", logN)
    title <- substitute(psi == integral(e^-x^2*dx,-infinity,infinity) 
                       #~~~~~~ hat(psi) == xxxpsihatxxx 
                       ~~~~~~ hat(psi)^2 == xxxpsihat2xxx
                       ~~~~~~ N == xxxNxxx,
                       list(#xxxpsihatxxx = integral_estimate_str, 
                            xxxpsihat2xxx = pi_estimate_str,
                            xxxNxxx = n_str))
    colors <- ifelse(inside, darkgreen, darkred)
    ylabel <- expression(f(x) == e^-x^2)

    # set margin mai = c(bottom, left, top, right)
    par(mai = c(0.6, 0.8, 1.5, 0.5))
    plot(x, y, 
         xlab = "", 
         ylab = "", 
         pch = marker, 
         col = colors, 
         xlim = c(-4, 4), 
         ylim = c(0, 1), 
         axes = FALSE,
         main = title,
         cex.main = fsf)
    axis(side = 1, labels = TRUE, at = -4:4, 
         font = 1, cex.axis = fsf)
    axis(side = 2, labels = TRUE, at = seq(0 , 1, 0.2), 
         font = 1, cex.axis = fsf, las =1)
    text(4.4, -0.04, "x", font = 1, xpd = TRUE, cex = fsf)
    text(-4.2, 1.07, ylabel, font = 1, xpd = TRUE, cex = fsf)

    # plot normal pdf f(x) = e^-x2
    x <- seq(-4, 4, length = 100)
    hx <- f(x)
    lines(x, hx)

    dev.off()
    return(c(pi_estimate, walltime))
}

# Perform MC integration
# ----------------------
print("N, pi_estimate")
pi_estimates <- c()
walltimes <- c()
logNs <- seq(1, endlogN)
for (logN in logNs) {
    retval <- mcint(logN)
    pi_estimate <- retval[1]
    pi_estimates <- c(pi_estimates, pi_estimate)
    walltime <- retval[2]
    walltimes <- c(walltimes, walltime)
}

# Plot convergence
# ----------------
if (plot_convergence) {

    plot.new()
    postscript("pi_convergence.eps", width = dim, height = dim, horizontal = FALSE)

    x <- 1 / logNs
    y <- pi_estimates
    subset_x <- x[-1:-length(x)/4]
    subset_y <- y[-1:-length(x)/4]
    subset_logNs <- logNs[-1:-length(logNs)/4]

    #ymin <- min(subset_y)
    ymin <- 0
    #ymax <- max(subset_y)
    ymax <- 4
    #xmin <- min(subset_x)
    xmin <- 0
    #xmax <- max(subset_x)
    xmax <- 0.3
    xlabel <- "1/logN"
    ylabel <- expression(hat(psi)^2)
    title <- "Convergence plot"

    # set margin mai = c(bottom, left, top, right)
    par(mai = c(1.0, 1.0, 1.0, 1.0))
    plot(subset_x, subset_y,
         xlab = "", 
         ylab = "", 
         pch = marker, 
         xlim = c(xmin, xmax),
         ylim = c(ymin, ymax),
         axes = FALSE,
         main = title,
         cex.main = fsf)

    axis(side = 1, labels = TRUE, at = seq(xmin, xmax, 0.05),
         font = 1, cex.axis = fsf)
    axis(side = 2, labels = TRUE, at = seq(ymin, ymax, 0.5), 
         font = 1, cex.axis = fsf, las = 1)
    text(xmax + 0.035, ymin - 0.15, xlabel, font = 1, xpd = TRUE, cex = fsf)
    text(xmin - 0.01, ymax + 0.3, ylabel, font = 1, xpd = TRUE, cex = fsf)

    # connect scatterplot points with dashed line
    lines(subset_x, subset_y, type = "l")

    # draw in line at y = pi
    xx <- c(0, 1)
    yy <- c(pi, pi)
    lines(xx, yy, xaxt = "n")
    dev.off()
}

# Plot walltime
# -------------
if (plot_walltime) {
    plot.new()
    postscript("pi_walltime.eps", width = dim, height = dim, horizontal = FALSE)

    x <- logNs
    y <- walltimes
    subset_x <- x[-1:-length(x)/4]
    subset_y <- y[-1:-length(x)/4]

    #ymin <- min(subset_y)
    ymin <- 0
    #ymax <- max(subset_y)
    ymax <- 2
    xmin <- min(subset_x)
    xmax <- max(subset_x)
    xlabel <- "logN"
    ylabel <- "walltime (s)"
    title <- "Walltime plot"

    # set margin mai = c(bottom, left, top, right)
    par(mai = c(1.0, 1.1, 1.0, 1.0))
    plot(subset_x, subset_y,
         xlab = "", 
         ylab = "", 
         pch = marker, 
         xlim = c(xmin, xmax),
         ylim = c(ymin, ymax),
         axes = FALSE,
         main = title,
         cex.main = fsf)

    axis(side = 1, labels = TRUE, at = subset_x,
         font = 1, cex.axis = fsf)
    axis(side = 2, labels = TRUE, at = seq(ymin, ymax, 0.5), 
         font = 1, cex.axis = fsf, las = 1)
    text(xmax + 1, ymin - 0.1, xlabel, font = 1, xpd = TRUE, cex = fsf)
    text(xmin, ymax + 0.15, ylabel, font = 1, xpd = TRUE, cex = fsf)

    # connect scatterplot points with dashed line
    lines(subset_x, subset_y, type = "l")

}
