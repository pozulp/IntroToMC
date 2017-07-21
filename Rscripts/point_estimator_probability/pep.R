#!/usr/bin/Rscript
# pep.R
# Point Estimator Probability (PEP)
# Use MC to solve for probabilities

fsf <- 2 # font size factor
dim <- 8 # dimension in inches of graphics region
darkred <- rgb(204/255, 0, 0)
darkgreen <- rgb(0, 153/255, 51/255)
darkblue <- rgb(0, 102/255, 255/255)
marker <- 20

N <- 10 ** 5
theta <- 10

x1 <- runif(N, 0, theta)
x2 <- runif(N, 0, theta)

# Must use apply() to compute entry-by-entry
# mean(x1, x2), min(x1, x2), and max(x1, x2)
xmat <- matrix(c(x1, x2), ncol = 2)
xbar <- apply(xmat, 1, mean)
xmax <- apply(xmat, 1, max)
xmin <- apply(xmat, 1, min)

est1 <- 2 * xbar
est2 <- 3 * xmax / 2
est3 <- 3 * xmin

diff1 = abs(est1 - theta)
diff2 = abs(est2 - theta)
diff3 = abs(est3 - theta)

diffmat <- matrix(c(diff1, diff2, diff3), ncol = 3)
closest <- apply(diffmat, 1, which.min)

p1hat <- sum(closest == 1) / length(closest)
p2hat <- sum(closest == 2) / length(closest)
p3hat <- sum(closest == 3) / length(closest)

result <- sprintf("%0.3f,%0.3f,%0.3f,%1.e", p1hat, p2hat, p3hat, N)
print(result)

palette <- c(darkred, darkgreen, darkblue)
colors <- palette[closest]

postscript("estimatorAreaPlot.eps", width = dim, height = dim, horizontal = FALSE)
#title <- substitute(hat(p)[1] == xxxp1hatxxx ~~~
#                   hat(p)[2] == xxxp2hatxxx ~~~
#                   hat(p)[3] == xxxp3hatxxx ~~~
#                   N == xxxNxxx,
#                   list(xxxp1hatxxx = sprintf("%0.3f", p1hat),
#                        xxxp2hatxxx = sprintf("%0.3f", p2hat),
#                        xxxp3hatxxx = sprintf("%0.3f", p3hat),
#                        xxxNxxx = N))

# set margin mai = c(bottom, left, top, right)
par(mai = c(0.6, 0.6, 1, 0.7))
plot(x1, x2, 
     xlab = "",
     ylab = "",
     pch = marker, 
     col = colors, 
     xlim = c(0,theta), 
     ylim = c(0,theta),
     axes = FALSE,
#     main = title,
     cex.main = fsf)

axis(side = 1, labels = TRUE, at = seq(0, theta, 2),
     font = 1, cex.axis = fsf)
axis(side = 2, labels = TRUE, at = seq(0, theta, 2), 
     font = 1, cex.axis = fsf, las =1)
text(theta + 0.8, -0.5, expression(X[1]), font = 1, xpd = TRUE, cex = fsf)
text(-0.5, theta + 0.6, expression(X[2]), font = 1, xpd = TRUE, cex = fsf)


# Make the title with estimators 
# colored the same as the plotted points
# to which they correspond using phantom,
# thank you plotmath!
p1title <- substitute(
                    hat(p)[1] == xxxp1hatxxx ~~            # p1 is only non-phantom
                    phantom(hat(p)[2] == xxxp2hatxxx) ~~
                    phantom(hat(p)[3] == xxxp3hatxxx) ~~
                    phantom(N == xxxNxxx),
                    list(xxxp1hatxxx = sprintf("%0.3f", p1hat),
                         xxxp2hatxxx = sprintf("%0.3f", p2hat),
                         xxxp3hatxxx = sprintf("%0.3f", p3hat),
                         xxxNxxx = sprintf("%1.e", N)))
p2title <- substitute(
                    phantom(hat(p)[1] == xxxp1hatxxx) ~~
                    hat(p)[2] == xxxp2hatxxx ~~            # p2 is only non-phantom
                    phantom(hat(p)[3] == xxxp3hatxxx) ~~
                    phantom(N == xxxNxxx),
                    list(xxxp1hatxxx = sprintf("%0.3f", p1hat),
                         xxxp2hatxxx = sprintf("%0.3f", p2hat),
                         xxxp3hatxxx = sprintf("%0.3f", p3hat),
                         xxxNxxx = sprintf("%1.e", N)))
p3title <- substitute(
                    phantom(hat(p)[1] == xxxp1hatxxx) ~~
                    phantom(hat(p)[2] == xxxp2hatxxx) ~~
                    hat(p)[3] == xxxp3hatxxx ~~            # p3 is only non-phantom
                    phantom(N == xxxNxxx),
                    list(xxxp1hatxxx = sprintf("%0.3f", p1hat),
                         xxxp2hatxxx = sprintf("%0.3f", p2hat),
                         xxxp3hatxxx = sprintf("%0.3f", p3hat),
                         xxxNxxx = sprintf("%1.e", N)))

Ntitle <- substitute(
                    phantom(hat(p)[1] == xxxp1hatxxx) ~~
                    phantom(hat(p)[2] == xxxp2hatxxx) ~~
                    phantom(hat(p)[3] == xxxp3hatxxx) ~~
                    N == xxxNxxx,                           # N is only non-phantom
                    list(xxxp1hatxxx = sprintf("%0.3f", p1hat),
                         xxxp2hatxxx = sprintf("%0.3f", p2hat),
                         xxxp3hatxxx = sprintf("%0.3f", p3hat),
                         xxxNxxx = sprintf("%1.e", N)))

title(p1title, col.main = darkred, cex.main = fsf)
title(p2title, col.main = darkgreen, cex.main = fsf)
title(p3title, col.main = darkblue, cex.main = fsf)
title(Ntitle, col.main = "black", cex.main = fsf)
