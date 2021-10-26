
N <- 100000
pop <- rnorm(N, 1, 2)
pop.mean <- mean(pop)
pop.sd <- sd(pop)
pop.median <- median(pop)
hist(pop, breaks = 400)

n.trials <- 10000
sample.size <- 10
sample.stat <- numeric(n.trials)

for (i in 1:n.trials) {
  samp <- sample(pop, sample.size, replace = T)
  sample.stat[i] <- mean(samp)
}
mean(sample.stat)
pop.mean
sd(sample.stat)
pop.sd/sqrt(10)


#lab 6
library(MASS)
#a)
make.fita <- function(r) {
  n <- 100
  dat <- mvrnorm(n, rep(0, 2), matrix(c(1, r, r, 1), 2, 2))
  x1 <- dat[, 1]
  x2 <- dat[, 2]
  y <- 1 + 2*x1 + 3 * x2
  dat <- data.frame(x1, x2, y)
  plot(dat)
  lm.1 <- lm(y ~ x1 + x2)
  return(summary(lm.1))
}
parta <- make.fit(.5)
plot(x1, x2)
plot(x1, y)
plot(x2, y)

#b)
parta$r.squared #r2
parta$sigma #Se

#c)
#same as part a, but correlation coeff = .1
make.fitc <- function(r) {
  n <- 100
  dat <- mvrnorm(n, rep(0, 2), matrix(c(1, r, r, 1), 2, 2))
  x1 <- dat[, 1]
  x2 <- dat[, 2]
  y <- 1 + 2*x1 + 3 * x2
  dat <- data.frame(x1, x2, y)
  plot(dat)
  lm.1 <- lm(y ~ x1 + x2)
  lm.2 <- lm(y ~ x1*x2)
  return(summary(lm.1))
}
partc <- make.fit(.1)
plot(x1, x2)
plot(x1, y)
plot(x2, y)

#d)
partc$r.squared #r2
partc$sigma #Se

#e)
parte <- summary(lm(y ~ x1*x2))
partc$r.squared #r2
partc$sigma #Se

#part2
#f)
N <- 100000
pop <- rnorm(N, 2, 3)
pop.mean <- mean(pop)
pop.sd <- sd(pop)

n.trials <- 10000
sample.size <- 500
sample.stat <- numeric(n.trials)

for (i in 1:n.trials) {
  samp <- sample(pop, sample.size, replace = T)
  sample.stat[i] <- sd(samp)
}
hist(sample.stat, breaks = 400)

#g)
qqnorm(sample.stat)
abline(3, .1)
#mean ~~ 3
#sd ~~ .1
#c ~~ .9