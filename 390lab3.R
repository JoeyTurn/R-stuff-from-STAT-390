set.seed(123)

#a)
a <- rexp(1000, 1.5)

#b)
quartiles <- boxplot(a, range = 0)

#c)
fivenumsummary <- quartiles$stats
fivenumsummary

##for (i in 1:5) {
##  abline(fivenumsummary[i, 1], 0)
##}

#d)
fivenumsummary2 <- numeric(5)
for (i in 1:5) {
  fivenumsummary2[i] <- quantile(a, (i-1)*.25)
}
fivenumsummary2

#Q0 = 0.0005506492, Q1 = 0.2044617954, Q2 = 0.4874444720, Q3 = 0.9510901076, Q4 = 4.8073383840

#e)
fivenumsummaryweird <- numeric(5)
fivenumsummaryweird[1] <- quantile(a, 0)
fivenumsummaryweird[2] <- quantile(a, .1)
fivenumsummaryweird[3] <- quantile(a, .5)
fivenumsummaryweird[4] <- quantile(a, .9)
fivenumsummaryweird[5] <- quantile(a, 1)
fivenumsummaryweird

#0 = 0.0005506492, .1 = 0.0657498214, .5 = 0.4874444720, .9 = 1.5199478317, 1 = 4.8073383840

#f)
fivenumsummarybetter <- log(fivenumsummaryweird)
fivenumsummarybetter

#-7.5044125 -2.7218983 -0.7185789  0.4186760  1.5701436

#g)

lastfivenumsummary <- numeric(5)
l_factor <- -(1/1.5)
lastfivenumsummary[1] <- l_factor*log(1-0)
lastfivenumsummary[2] <- l_factor*log(1-.1)
lastfivenumsummary[3] <- l_factor*log(1-.5)
lastfivenumsummary[4] <- l_factor*log(1-.9)
lastfivenumsummary[5] <- l_factor*log(1-1)
lastfivenumsummary

#0.00000000 0.07024034 0.46209812 1.53505673 Inf.

##PART 2

#h)
h <- read.table("https://www.stat.washington.edu/marzban/390/spring20/brainhead_dat.txt", header=T)
boxplot(split(h$brain, h$gender))

#i)
par(mfrow = c(3, 4))
x <- 1000
hist(rbinom(x, 5, .01))
hist(rbinom(x, 5, .1))
hist(rbinom(x, 5, .5))
hist(rbinom(x, 5, .9))
hist(rbinom(x, 10, .01))
hist(rbinom(x, 10, .1))
hist(rbinom(x, 10, .5))
hist(rbinom(x, 10, .9))
hist(rbinom(x, 20, .01))
hist(rbinom(x, 20, .1))
hist(rbinom(x, 20, .5))
hist(rbinom(x, 20, .9))
