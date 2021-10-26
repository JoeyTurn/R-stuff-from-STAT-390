#library(dplyr)
#a)
dat <- read.csv("https://www.stat.washington.edu/marzban/390/spring20/qz2_dat.txt", header = FALSE, sep = " ")
#dim(dat) <- checking I got 4 columns

#b)
x1 <- dat[, 1]
x2 <- dat[, 2]
x3 <- dat[, 3]
x4 <- dat[, 4]

#c)
par(mfrow=c(1,2))
hist(x1, breaks = 80)
hist(x2, breaks = 80)
#because x1 is so clearly bell shaped, we can assume (from the question) that x2 is bimodal
#x1 <- bell shaped, x2 <- bimodal

#didn't know if this was part of question, so it's commented
#par(mfrow=c(1,3))
#H <- hist(x1, breaks = 100) # Direct the output of hist() into something called H. 
#names(H) # Shows all the items in H
#plot(H$mids, H$counts)
#plot(H$mids, H$counts, type="h")
#par(mfrow = c(1,1))
#plot(H$mids, log(H$counts) )

#d)
par(mfrow=c(2, 2))
H3 <- hist(x3, breaks = 100)
plot(H3$mids, log(H3$counts), type = "h")
H4 <- hist(x4, breaks = 100)
plot(H4$mids, log(H4$counts), type = "h")
#x3 is exponential because the plot for x3 vs log(frequency) resembles frequency = -a*x

#e)
plot(log(H4$mids), log(H4$counts), type = "h")
#x4 follows the power law because the plot for log(x4) vs log(frequency) resembles frequency = -a*x