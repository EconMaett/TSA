## calculate eigenvalues for various autoregressive processes
## Kevin Kotze - Jul '16

## clear data & close graphs
rm(list = ls())
graphics.off()

## specify different difference equations
dif.eq.AR1a <- c(1, -0.9)
dif.eq.AR1b <- c(1, 1.1)
dif.eq.AR2a <- c(1, -0.6, -0.2)
dif.eq.AR2b <- c(1, 0.7, +0.8)
dif.eq.AR3a <- c(1, -0.6, 1.1, -0.2)
dif.eq.AR3b <- c(1, 0.7, 1.5, 0.1)

## Eigenvalues - take the reverse of the characteristic roots
eigen.AR1a <- polyroot(rev(dif.eq.AR1a))
eigen.AR1b <- polyroot(rev(dif.eq.AR1b))
eigen.AR2a <- polyroot(rev(dif.eq.AR2a))
eigen.AR2b <- polyroot(rev(dif.eq.AR2b))
eigen.AR3a <- polyroot(rev(dif.eq.AR3a))
eigen.AR3b <- polyroot(rev(dif.eq.AR3b))

## Plot plane for eigenvectors
layout(matrix(c(1), 1, 1, byrow = TRUE))
x <- seq(-1, 1, length = 200)
y1 <- sqrt(1 - x ^ 2)
y2 <- -sqrt(1 - x ^ 2)
plot(
  c(x, x),
  c(y1, y2),
  xlab = 'Real part',
  ylab = 'Complex part',
  type = 'l',
  main = 'Unit Circle',
  ylim = c(-2, 2),
  xlim = c(-2, 2)
)
abline(h = 0)
abline(v = 0)

## Plot eigenvalues
points(Re(polyroot(rev(dif.eq.AR2a))),
       Im(polyroot(rev(dif.eq.AR2a))), pch = 19, col = "red")
legend(
  -2,
  -1.3,
  legend = "Eigenvalues of AR(2)",
  pch = 19,
  col = "red",
  bty = "n"
)

## Look at the real parts separately
result <- data.frame(matrix(rep(NaN, 6 * 6), ncol <- 6))
result[1, 1] <- Re(eigen.AR1a)
result[1, 2] <- Im(eigen.AR1a)
result[2, 1] <- Re(eigen.AR1b)
result[2, 2] <- Im(eigen.AR1b)
result[3, 1] <- Re(eigen.AR2a)[1]
result[3, 2] <- Im(eigen.AR2a)[1]
result[3, 3] <- Re(eigen.AR2a)[2]
result[3, 4] <- Im(eigen.AR2a)[2]
result[4, 1] <- Re(eigen.AR2b)[1]
result[4, 2] <- Im(eigen.AR2b)[1]
result[4, 3] <- Re(eigen.AR2b)[2]
result[4, 4] <- Im(eigen.AR2b)[2]
result[5, 1] <- Re(eigen.AR3a)[1]
result[5, 2] <- Im(eigen.AR3a)[1]
result[5, 3] <- Re(eigen.AR3a)[2]
result[5, 4] <- Im(eigen.AR3a)[2]
result[5, 5] <- Re(eigen.AR3a)[3]
result[5, 6] <- Im(eigen.AR3a)[3]
result[6, 1] <- Re(eigen.AR3b)[1]
result[6, 2] <- Im(eigen.AR3b)[1]
result[6, 3] <- Re(eigen.AR3b)[2]
result[6, 4] <- Im(eigen.AR3b)[2]
result[6, 5] <- Re(eigen.AR3b)[3]
result[6, 6] <- Im(eigen.AR3b)[3]
