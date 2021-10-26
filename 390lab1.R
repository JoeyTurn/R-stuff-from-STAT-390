#a)
data("morley")

#b)
dim(morley)
#100, 3

#c)
names(morley)[3]
#"Speed"

#d)
mean(morley[,3])
#852.4

#e)
mean(morley[1:20,3])
#909

#f)
par(mfrow = c(1, 1))
hist(morley[,3], breaks = 20)

#g)
abline(v=792.458,col=4) #i like blue
