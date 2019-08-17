oring <- read.table('http://www.randomservices.org/random/data/Challenger2.txt', header = T)
attach(oring)
oring
plot(T,I)

# Frequentist approach
oring.lm = lm (I ~ T)
summary(oring.lm)
lines(T, fitted(oring.lm))

coef(oring.lm)
predict(oring.lm, data.frame(T=31), interval = 'predict')
