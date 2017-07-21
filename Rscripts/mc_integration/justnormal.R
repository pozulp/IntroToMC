#!/bin/Rscript
# justnormal.R

fsf <- 2 # font size factor
dim <- 8 # dimension in inches of graphics region

f <- function(x) {
    exp(-(x*x))
}

plotname <- "justnormal.eps"
postscript(plotname, width = dim, height = dim, horizontal = FALSE)

# plot normal pdf f(x) = e^-x2
x <- seq(-4, 4, length=100)
hx <- f(x)
title <- expression(
         ~ "Shaded area " %~~%
         integral(e^-x^2*dx,-infinity,infinity) 
         )
ylabel <- expression(f(x) == e^-x^2)

# set margin mai = c(bottom, left, top, right)
par(mai = c(0.6, 0.7, 0.9, 0.3))
plot(x, hx, 
     type = "l",
     lwd = 1.5,
     xlab = "",
     ylab = "",
     xlim = c(-4, 4),
     ylim = c(0, 1),
     axes = FALSE,
     main=title,
     cex.main=fsf)
axis(side = 1, labels = TRUE, at = -4:4, font = 1, cex.axis = fsf)
axis(side = 2, labels = TRUE, at = seq(0,1,0.2), font = 1, cex.axis = fsf, las = 1)
text(4.4, -0.04, "x", font = 1, xpd = TRUE, cex = fsf)
text(-4.2, 1.08, ylabel, font = 1, xpd = TRUE, cex = fsf)

# Shade underneath curve
xpoly <- c(x, -4, 4)
ypoly <- c(hx, 0, 0)
polygon(xpoly, ypoly, col = gray(0.5))
