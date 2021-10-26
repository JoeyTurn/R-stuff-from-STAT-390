dat <- read.table("https://www.stat.washington.edu/marzban/390/spring20/11_39_dat.txt", header=T)

plot(dat, cex = .5)
model.1 <- lm(y~x1 + x2, data = dat)
summary(model.1)
summary(model.1)$fstatistic
summary(model.1)$fstatistic[1]
confint(model.1, level = .99)
predict(model.1, interval = "confidence", level = .95)
predict(model.1, interval = "prediction", level = .95)
predict(model.1, newdata = new.dat, interval = "prediction", level = .95)

#actual quiz

#a)
sample.size <- 100
n <- 5000
x <- 1:100
y <- 20 + 2*x + .1*(x^2)
sigma_epsilon <- 15
alpha_hat <- beta1_hat <- beta2_hat <- numeric(n)
set.seed(123)
for (i in 1:n) {
  samp <- sample(x, sample.size)
  sampy <- 20 + 2*samp + .1*(samp^2)
  lm.1 <- lm(sampy ~ samp + I(samp^2))
  alpha_hat[i] <- lm.1$coefficients[1]
  beta1_hat[i] <- lm.1$coefficients[2]
  beta2_hat[i] <- lm.1$coefficients[3]
}

#b)
mean(alpha_hat);sd(alpha_hat)^2 #20;9.057984e-26
mean(beta1_hat);sd(beta1_hat)^2 #2;5.166858e-29
mean(beta2_hat);sd(beta2_hat)^2 #.1;2.875488e-33

#c)
sigma_epsilon^2/((sample.size-1)*sd(x)) #0.07833886

#d)
dat <- read.table("https://www.stat.washington.edu/marzban/390/spring20/qz10_dat.txt", header=T)
plot(dat, cex = .5)
model.1 <- lm(X.0.4401629~X.0.424845 * X0.1999779, data = dat)
summary(model.1) #p-val = 1.45e-05
#p-val: <.001. Because the model we tested had an interaction term & p < .05, we can conclude
#              that an interaction term does exist

#e)
preds <- predict(model.1, interval = "prediction", level = .95)
pis <- 0
for (i in 1:99) {
  if(dat[i, 3] <= preds[i, 3] && dat[i, 2] >= preds[i, 2]) {
    pis = pis + 1
  }
}
pis #94
