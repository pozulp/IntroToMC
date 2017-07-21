#!/bin/Rscript
# exponentialPlots.R

fsf <- 2 # font size factor
dim <- 8 # dimension in inches of graphics region
lwd <- 4 # line width
titlefsf <- 4 # title font size factor

setupPlot <- function(plotname, eps) {

    if (eps) {
        fullname <- paste(plotname, ".eps", sep="")
        postscript(fullname, width = dim, height = dim, horizontal = FALSE)
    }
    else {
        fullname <- paste(plotname, ".png", sep="")
        png(fullname)
    }
}

doPlot <- function(x, y, title, invertAxesLimits) {

    xmin <- 0
    xmax <- 5
    xstep <- 1
    ymin <- 0
    ymax <- 1
    ystep <- 0.2

    if (invertAxesLimits) {
        xmax <- 1
        xstep <- 0.2
        ymax <- 5
        ystep <- 1
    }

    #plot.new()
    # set margin mai = c(bottom, left, top, right)
    par(mai = c(0.6, 0.7, 1, 0.3))
    plot(x, y, 
         type = "l",
         lwd = lwd,
         xlab = "",
         ylab = "",
         xlim = c(xmin, xmax),
         ylim = c(ymin, ymax),
         axes = FALSE,
         main=title,
         cex.main=titlefsf)
    axis(side = 1, labels = TRUE, at = seq(xmin, xmax, xstep), font = 1, cex.axis = fsf)
    axis(side = 2, labels = TRUE, at = seq(ymin, ymax, ystep), font = 1, cex.axis = fsf, las = 1)
    #text(5.17, -0.04, xlabel, font = 1, xpd = TRUE, cex = fsf)
    #text(-0.1, 1.06, ylabel, font = 1, xpd = TRUE, cex = fsf)
}

plotExponentialPDF <- function(plotname, title, eps) {

    setupPlot(plotname, eps)

    # plot exponential pdf f(x) = e^-x
    x <- seq(0, 5, length=100)
    y <- dexp(x)

    doPlot(x, y, title, FALSE)
}

plotExponentialCDF = function(plotname, title, eps) {

    setupPlot(plotname, eps)

    # plot exponential cdf F(x) = 1 - e^-x
    x <- seq(0, 5, length=100)
    y <- pexp(x)

    doPlot(x, y, title, FALSE)
}

plotExponentialInvCDF = function(plotname, title, eps) {

    setupPlot(plotname, eps)

    # plot exponential cdf F(x) = 1 - e^-x
    x <- seq(0, 1, length=1000)
    y <- qexp(x)

    doPlot(x, y, title, TRUE)

    # draw vertical asymptote at x = 5
    abline(v = 1, lty = 2, lwd = lwd)

}

eps <- TRUE
pdfTitle <- expression(f(x) == e^-x)
cdfTitle <- expression(F(x) == 1 - e^-x)
invCDFTitle <- expression(F^{-1} * (u) == -ln(1 - u))
plotExponentialPDF("expPDF", pdfTitle, eps)
plotExponentialCDF("expCDF", cdfTitle, eps)
plotExponentialInvCDF("expInvCDF", invCDFTitle, eps) 
