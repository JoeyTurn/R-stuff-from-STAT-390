H <- c(1.2, 0.9, 0.7, 1.0, 1.7, 1.7, 1.1, 0.9, 1.7, 1.9, 1.3, 2.1, 1.6, 1.8, 1.4, 1.3, 1.9, 1.6, 0.8, 2.0, 1.7, 1.6, 2.3, 2.0)
P <- c(1.6, 1.5, 1.1, 2.1, 1.5, 1.3, 1.0, 2.6)
t.test(H)
t.test(P)
boxplot(H, P, names = c("high-quality", "low-quality"))
t.test(H, P, alternative = "two.sided")
t.test(H, P, alternative = "less")
t.test(H, P, alternative = "greater")
t.test(H, P)$p.value
print(t.test(H, P)$p.value)

H <- H[1:8]
boxplot(H, P, names = c("high-quality", "low-quality"))
t.test(H, P, alternative = "two.sided")
t.test(H, P, paired = T, alternative = "two.sided")
plot(H, P)

weight <- c(14.6, 14.4, 19.5, 24.3, 16.3, 22.1, 23, 18.7, 19, 17, 19.1, 19.6, 23.2, 18.5, 15.9)
tread <- c(11.3, 5.4, 9.1, 15.2, 10.1, 19.6, 20.8, 10.3, 10.3, 2.6, 16.6, 22.4, 23.6, 12.6, 4.4)
boxplot(weight, tread, names = c("weight", "treadmill"))
plot(weight, tread)
cor(weight, tread)
qqnorm(weight)
qqnorm(tread)
qqnorm(weight-tread)
t.test(weight, tread, paired = T, alternative = "greater")
t.test(weight, tread, mu = 5, paired = T, alternative = "greater")
t_obs <- (mean(weight-tread) - 5) / (sd(weight-tread) / sqrt(15))
pt(t_obs, lower.tail = F, df = 14)

mu.1 <- 0
mu.2 <- 0
n.trials <- 1000
p <- numeric(n.trials)
for (i in 1:n.trials) {
  x1 <- rnorm(100, mu.1)
  x2 <- rnorm(100, mu.2)
  p[i] <- t.test(x1, x2)$p.value
}
hist(p, breaks = 20, xlim = c(0, 1))
range(p)
