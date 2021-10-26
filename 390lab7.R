N <- 100000
pop <- rnorm(N, 1, 2)

pop.mean <- mean(pop)
pop.sd <- sd(pop)
pop.median <- median(pop)
c(pop.mean, pop.sd, pop.median)
sample.size <- 200
sample.trial <- sample(pop, sample.size, replace = T)
sample.stat <- mean(sample.trial)
std.err <- sd(sample.trial) / sqrt(sample.size)
sample.stat - abs(qnorm(.05/2))*std.err
sample.stat + abs(qnorm(.05/2))*std.err
t.test(sample.trial, alternative = "two.sided", conf.level = .95)

n.trial <- 100
sample.size <- 90
CI <- matrix(nrow = n.trial, ncol = 2)
for (i in 1:n.trial) {
  sample.trial <- sample(pop, sample.size)
  CI[i, ] <- t.test(sample.trial)$conf.int[1:2]
}

count <- 0
for (i in 1:n.trial) {
  if (CI[i, 1] <= pop.mean && CI[i, 2] >= pop.mean) {
    count <- count + 1
  }
}
count

plot(c(1, 1), CI[1, ], ylim = c(0, 2), xlim = c(0, 101), ylab = "CI", xlab = '')
for(i in 2:n.trial) {
  lines(c(i, i), CI[i, ])
}
abline(h = pop.mean, col = "red", lwd = 3)

#t stuff
pnorm(1.645, 0, 1, lower.tail = T)
pt(1.645, df = 5, lower.tail = F)
qnorm(.05, 0, 1, lower.tail = T)
qt(0.05, df = 5, lower.tail = T)

x <- seq(-5, 5, .1)
y_1 <- dnorm(x, 0, 1)
y_2 <- dt(x, 2)
y_3 <- dt(x, 5)
plot(x, y_1, ylab = 'y')
lines(x, y_2, col = 2)
lines(x, y_3, col = 4)

dat <- read.table("https://www.stat.washington.edu/marzban/390/spring20/attend_dat.txt", header=T)
attendance <- dat[, 1]
gender <- dat[, 2]
pa.boy <- attendance[gender == 0]
pa.girl <- attendance[gender == 1]

n.boys <- length(pa.boy)
n.girls <- length(pa.girl)
mean(pa.boy)
mean(pa.girl)
t.test(pa.boy)$conf.int[1:2]
t.test(pa.girl)$conf.int[1:2]
t.test(pa.boy, pa.girl, alternative = "two.sided")

a<-numeric(10)
for (i in 1:10) {
  a[i] <- (5.5 - abs(i-5.5))/30
}
b <- c(4,  15,  23, 25, 38, 31, 32, 14, 10, 8)
tot <- numeric(10)
for (i in 1:10) {
  tot[i] = (a[i]*200-b[i])^2/(a[i]*200)
}
