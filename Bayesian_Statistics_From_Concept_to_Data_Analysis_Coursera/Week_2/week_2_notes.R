likelihood = function(n,y,theta){return(theta^y*(1-theta)^(n-y))}
theta = seq(from = 0.01, to = 0.99, by = 0.01)
plot(theta, likelihood(400,72, theta))
72/400
abline(v = 0.18)
loglike = function(n,y,theta){return(y*log(theta)+(n-y)*log(1-theta))}
plot(theta, loglike(400, 72, 
                    theta))
abline(v=0.18)

# Let X ∼ Pois(3). Find P(X = 1)
dpois(x = 1, lambda = 3)
# Let X ∼ Pois(3). Find P(X ≤ 1)
ppois(q = 1, lambda = 3)
# Let X ∼ Pois(3). Find P(X > 1)
1 - ppois(q = 1, lambda =3)
# Let Y ∼ Gamma(2, 1/3). Find P(0.5 < Y < 1.5)
pgamma(q = 1.5,shape = 2, rate = 1/3) - pgamma(q = 0.5,shape = 2, rate = 1/3) 
# Let Z ∼ N(0, 1). Find z such that P(Z < z) = 0.975
qnorm(p = 0.975, mean = 0, sd = 1)
# Let Z ∼ N(0, 1). Find P(−1.96 < Z < 1.96)
pnorm(q = 1.96, mean = 0, sd = 1) - pnorm(q = -1.96, mean = 0, sd = 1)
# Let Z ∼ N(0, 1). Find z such that P(−z < Z < z) = 0.90
qnorm(p = c(0.05, 0.95), mean = 0, sd = 1)
