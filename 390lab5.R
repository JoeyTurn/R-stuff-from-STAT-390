#a)
# reading in data:
brain <- read.table("https://www.stat.washington.edu/marzban/390/spring20/brainhead_dat.txt", header=T)
# performing regression:
lm.1 <- lm(brain$brain ~ brain$head)
# returning OLS intercept and slope:
lm.1$coefficients
# plotting scatterplot and the OLS line:
plot(brain$head, brain$brain)
abline(lm.1)

#b)
##SSE == SS_unexp, SSR == SS_exp
SSE <- sum((lm.1$residuals)^2)
SST <- sum((brain$brain-mean(brain$brain))^2)
SSR <- sum((predict(lm.1)-mean(brain$brain))^2)

SST-SSR-SSE #basically 0

#c)
colnames(brain) <- c('t1', 't2', 'x', 'y')

#d)
x <- brain[1:100, 3]
y <- brain[1:100, 4]

#e)
lm.2 <- lm(y ~ x)

#f)
y_hat <- predict(lm.2, newdata = brain[101:237, 3:4])

#g)
new_y <- brain[101:237, 4]
SSE.2 <- sum((y-predict(lm.2))^2)
SST.2 <- sum((y-mean(y))^2)
SSR.2 <- sum((y_hat-mean(new_y))^2)

summary(lm.2)
SST.2 - SSR.2 - SSE.2
