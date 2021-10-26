obs.counts <- c(14, 28, 44)
p0 <- c(12/54, 17/54, 25/54)
chisq.test(obs.counts, p = p0)
chisq.test(obs.counts, p = p0)$p.value
chisq.test(obs.counts, p = p0)$expected
chisq.test(obs.counts, p = p0)$residuals^2

obs.counts = matrix(c(728, 1304, 495, 1072, 2800, 1993), ncol = 3, byrow = T)
chisq.test(obs.counts)
chisq.test(obs.counts)$expected
chisq.test(obs.counts)$residuals^2
obs.counts[2, ] / apply(obs.counts, 2, sum) # shows moderateness(row 2) increases with education (columns)

dat <- read.table("https://www.stat.washington.edu/marzban/390/spring20/9_1_dat.txt", header=T)

aov.1 = aov(Vibration ~ as.factor(Brand), data = dat)
summary(aov.1)
boxplot(Vibration ~ Brand, data = dat)

attach(dat) #loads all variables in a data set
k <- 5
n <- m <- s <- numeric(k)
for (i in 1:k) {
  n[i] <- 6
  m[i] <- mean(dat[Brand == i, 2])
  s[i] <- sd(dat[Brand == i, 2])
}
n
m
s
df.1 <- k-1
df.2 <- k*6 - k
SSB <- sum(n*(m-mean(m)) ^ 2)
SSW <- sum((n-1) * s^2)
MSB <- SSB / df.1
MSW <- SSW / df.2
F <- MSB/MSW
p.value <- 1-pf(F, df.1, df.2)

df.1;df.2;SSB;SSW;MSB;MSW;F;p.value

obs.counts <- matrix(c(435, 58, 89, 375, 50, 84), ncol = 3, byrow = T)
total <- sum(obs.counts)
rowsum <- apply(obs.counts, 1, sum)
colsum <- apply(obs.counts, 2, sum)
expecteed <- (matrix(rowsum) %*% t(matrix(colsum))) / total
expected

residuals <- (obs.counts - expected) / sqrt(expected)
df <- prod(dim(obs.counts) - 1)
X2 <- sum(residuals^2)
1-pchisq(X2, df)


#Quiz
#a)
#H0: p1 = p2 = ... = p10 = 1/10 (i use p for the population proportion)
#H1: One or more of the above proportions is incorrect

#b)
pos <- c(8, 9, 12, 4, 13, 17, 10, 9, 9, 9)
p0 <- c(1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10)
chisq.test(pos, p = p0)
chisq.test(pos, p = p0)$p.value #0.3041258 
#Because .3 > .05, there is insufficient evidence for H1 and thus we fail to reject H0

#c)
df.1 <- 4
df.2 <- 25
F <- 8.45
p.value <- 1-pf(F, df.1, df.2)
p.value #0.0001862396

#d)
criticalF <- qf(.05, df.1, df.2, lower.tail = FALSE)
criticalF #2.75871

#e)
set.seed(123)
n = 10
x = c(rep(1,n), rep(2,n), rep(3,n), rep(4,n) )
y = c(rnorm(n,0,1), rnorm(n,0,1), rnorm(n,0,1), rnorm(n,0,1) )
dat = cbind(x,y)       # print this to your screen and note the similarity (in structure) to the data used in the prelab,
k <- 4
n <- m <- s <- numeric(k)
for (i in 1:k) {
  n[i] <- 10
  m[i] <- mean(dat[x == i, 2])
  s[i] <- sd(dat[x == i, 2])
}
n;m;s
df.1 <- k-1
df.2 <- k*10 - k
SSB <- sum(n*(m-mean(m)) ^ 2)
SSW <- sum((n-1) * s^2)
MSB <- SSB / df.1
MSW <- SSW / df.2
Frat <- MSB/MSW
p.value <- 1-pf(Frat, df.1, df.2)
p.value #0.2635444

#f)
set.seed(123)
N <- 10000
frats <- SSB <- SSW <- MSB <- MSW <- numeric(N)
for (j in 1:N) {
  n <- 10
  x = c(rep(1,n), rep(2,n), rep(3,n), rep(4,n) )
  y = c(rnorm(n,0,1), rnorm(n,0,1), rnorm(n,0,1), rnorm(n,0,1) )
  dat = cbind(x,y)       # print this to your screen and note the similarity (in structure) to the data used in the prelab,
  k <- 4
  n <- m <- s <- numeric(k)
  for (i in 1:k) {
    n[i] <- 10
    m[i] <- mean(dat[x == i, 2])
    s[i] <- sd(dat[x == i, 2])
  }
  df.1 <- k-1
  df.2 <- k*10 - k
  SSB[j] <- sum(n*(m-mean(m)) ^ 2)
  SSW[j] <- sum((n-1) * s^2)
  MSB[j] <- SSB[j] / df.1
  MSW[j] <- SSW[j] / df.2
  frats[j] <- MSB[j]/MSW[j]
}
quantile(frats, probs = .95) #2.858567
