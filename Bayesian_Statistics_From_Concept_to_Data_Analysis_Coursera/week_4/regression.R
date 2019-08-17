#http://www.randomservices.org/random/data/Challenger2.txt
# 23 previous space shuttle launches before the Challenger disaster
# T is the temperature in Fahrenheit, I is the O-ring damage index

oring=read.table("http://www.randomservices.org/random/data/Challenger2.txt",header=T)
attach(oring)
#note: masking T=TRUE

plot(T,I)

oring.lm=lm(I~T)
summary(oring.lm)

# add fitted line to scatterplot
lines(T,fitted(oring.lm))            
# 95% posterior interval for the slope
-0.24337 - 0.06349*qt(.975,21)
-0.24337 + 0.06349*qt(.975,21)
# note that these are the same as the frequentist confidence intervals

# the Challenger launch was at 31 degrees Fahrenheit
# how much o-ring damage would we predict?
# y-hat
18.36508-0.24337*31
coef(oring.lm)
coef(oring.lm)[1] + coef(oring.lm)[2]*31  

# posterior prediction interval (same as frequentist)
predict(oring.lm,data.frame(T=31),interval="predict")  
10.82052-2.102*qt(.975,21)*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))

# posterior probability that damage index is greater than zero
1-pt((0-10.82052)/(2.102*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))),21)


#http://www.randomservices.org/random/data/Galton.txt
# Galton's seminal data on predicting the height of children from the 
# heights of the parents, all in inches

heights=read.table("http://www.randomservices.org/random/data/Galton.txt",header=T)
attach(heights)
names(heights)

pairs(heights)
summary(lm(Height~Father+Mother+Gender+Kids))
summary(lm(Height~Father+Mother+Gender))
heights.lm=lm(Height~Father+Mother+Gender)

# each extra inch taller a father is is correlated with 0.4 inch extra height in the child
# each extra inch taller a mother is is correlated with 0.3 inch extra height in the child
# a male child is on average 5.2 inches taller than a female child
# 95% posterior interval for the the difference in height by gender
5.226 - 0.144*qt(.975,894)
5.226 + 0.144*qt(.975,894)

# posterior prediction interval (same as frequentist)
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="M"),interval="predict")
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="F"),interval="predict")



data <- read.table('http://users.stat.ufl.edu/~winner/data/pgalpga2008.dat')
attach(data)
head(data)
names(data) = c("distance", "accuracy", "FM")
dataF <- subset(data, FM==1, select=1:2)
dataM <- subset(data, FM==2, select=1:2)

plot(dataF$distance, dataF$accuracy)
plot(dataM$distance, dataM$accuracy)

golf.lm.f <- lm(accuracy~ distance, data = dataF)
summary(golf.lm.f)
coef(golf.lm.f)[1] + coef(golf.lm.f)[2]*260  

predict(golf.lm.f,data.frame(distance=260),interval="predict")  
