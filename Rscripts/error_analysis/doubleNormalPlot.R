#!/bin/Rscript
# doubleNormalPlot.R

fsf <- 2 # font size factor
dim <- 8 # dimension in inches of graphics region

#plotname <- "doubleNormalPlot.png"
plotname <- "doubleNormalPlot.eps"
postscript(plotname, width = dim, height = dim, horizontal = FALSE)
#png(plotname)

# plot normal pdf f(x) = e^-x2
x <- seq(-6, 6, length=100)
y1 <- dnorm(x, sd = 1)
y2 <- dnorm(x, sd = 2)

y1label <- expression(paste(f(x[1]), ", ", X[1] %~% N(0, sigma == 1)))
y2label <- expression(paste(f(x[2]), ", ", X[2] %~% N(0, sigma == 2)))

# set margin mai = c(bottom, left, top, right)
par(mai = c(0.6, 0.7, 0.5, 0.3))
plot(x, y1, 
     type = "l",
     lwd = 1.5,
     xlab = "",
     ylab = "",
     xlim = c(-6, 6),
     ylim = c(0, 0.4),
     axes = FALSE,
     #main=title,
     cex.main=fsf)
lines(x, y2)
axis(side = 1, labels = TRUE, at = seq(-6, 6, 2), font = 1, cex.axis = fsf)
axis(side = 2, labels = TRUE, at = seq(0,0.4,0.1), font = 1, cex.axis = fsf, las = 1)
text(0, 0.43, y1label, font = 1, xpd = TRUE, cex = fsf)
text(0, 0.22, y2label, font = 1, xpd = TRUE, cex = fsf)
