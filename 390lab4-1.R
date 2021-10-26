## 2.4

n.trials <- 1000
n <- 1:100
sample.mean <- numeric(100)
sample.var <- numeric(100)
for (i in n) {
  head.counts <- rbinom(n.trials, i, .5)
  sample.mean[i] <- mean(head.counts)
  sample.var[i] <- var(head.counts)
}
plot(n, sample.mean, cex = .5)
points(n, sample.var, col = 2, cex = .5)

legend('topleft', c('Sample Mean', 'Variance'), text.col = c('black', 'red'), bty = 'n')

qnorm(.9, lower.tail = TRUE)
sequence <- seq(.1, .9, .1)
qnorm(sequence, lower.tail = TRUE)

## 2.7

n <- 500
x <- rnorm(n)
qqnorm(x, cex = .5)

#by hand
n <- 500
X <- seq(.5/n, 1-.5/n, length = n)
Q <- qnorm(X)
plot(Q, sort(x), col = 2, cex = .5)
abline(0,1)

n <- 500
x <- rnorm(n, 8, 2)
par(mfrow = c(1,2))
hist(x)

qqnorm(x, cex = .5)
abline(mean(x), sd(x))

x <- rexp(n)
hist(x)
qqnorm(x, cex = .5)

library(lattice)
x <- rexp(500)
par(mfrow = c(1,1))
hist(x)
qqmath(x, dist = "qexp")

dat <- read.table('https://www.stat.uw.edu/marzban/390/spring20/hist_dat.txt', header = F)
qqnorm(dat[, 1], cex = .5)
