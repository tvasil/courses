# Suppose we're giving two students a multiple-choice exam with 40 questions
# where each question has four choices. We don't know how much the students 
# have studied for this exam, but we think that they will do better than just
# guessing randomly.
# 1. What are the parameters of interest?
# 2. What is our likelihood?
# 3. What prior should we use?
# 4. What is the prior probability P(theta > .25)? P(theta > .5)? P(theta > .8)?
# 5. Suppose the first students gets 33 questions right. What is the posterior
#    distribution for theta1? P(theta1>.25)? P(theta1 > .5)? P(theta1 > .8)
#    What is a 95% posterior credible interval for theta1?
# 6. Suppose the second student gets 24 questions right. What is the posterior
#    distrition for theta2? P(theta2 > .25)? P(theta2 > .5)? P(theta2 > .8)
#    What is a 95% posterior credible interval for theta2?
# 7. What is the posterior probability that theta2>theta2, i.e that the 
#    first student has a better chance of getting a question right than
#    the second student?

################
###### ANSWERS #######
# 1. Parameters of interest are theta1 = true probability that the first student 
#    will answer a question correctly and theta2 = true probability that the second
#    student will answer a question correctly.

# 2. Likelihood is Binomial(40, theta), if we assume that each question is 
#    independent and that the probabiity a student gets each question right
#    is the same for all questions for that student

# 3. The conjugate prior is a beta prior. Plot the density with dbeta
# we're gonna guess on average students will be doing on the exam better than random, 
# so let's say 2/3 on average (this is just a guess!)
theta = seq(from=0, to = 1, by = 0.01)
plot(theta, dbeta(theta,1,1), type = 'l') # this is default prior
plot(theta, dbeta(theta, 4,2), type = 'l')
plot(theta, dbeta(theta, 8,4), type = 'l')  #this seems reasonable

# 4.Using the PDF:
1-pbeta(0.25, 8,4)
1-pbeta(.5, 8, 4)
1-pbeta(.8, 8,4)


# 5. Posterior distribution of beta for the first student?
41/(41+11) # posterior mean

33/40 # MLE
lines(theta, dbeta(theta, 41, 11)) # solid line shows the posterior

# as we get more information, the distribution gets more concentrated

# Solid line in the posterior
plot(theta, dbeta(theta, 41, 11), type = 'l')
# dashed line in the prior
lines(theta, dbeta(theta, 8,4), lty=2) 

# and this one is the likelihood
lines(theta, dbinom(33, size = 40, p = theta), lty = 3)

# but because it doesn't have any normalizing constant, it's almost 
# on a different scale, so we can't see it
# we need to scale it up by a factor of 44 (that's just a random number)
lines(theta, 44*dbinom(33, size = 40, p = theta), lty = 3)

# how about posterior probabilities?
# probability that theta is > 1/4
1- pbeta(.25, 41, 11)
# Probability that theta is > 1/2
1 - pbeta(.5, 41, 11)
# probability that theta is > 0.8
1- pbeta(.8, 41, 11)

# What is a 95% credible interval?
qbeta(.025, 41, 11)
qbeta(.975, 41, 11)

# this is also consistent with the mass on the graph
# most of the mass is between 0.65 and 0.9 (it appears)


# 5. Posterior distribution of beta for the second student?

# Since we assumed a prior Beta (a = 8, b = 4)
# now the posterior will be ~Beta (a + Sigma(yi), beta + n - Sigma (yi))
# = Beta(8+24, 4+40-24)
# = Beta(32, 20)

32/ (32+20) # posterior mean
24/40       # MLE

plot(theta, dbeta(theta, 32, 20), type = 'l') #posterior
lines(theta, dbeta(theta, 8, 4), lty = 2) # prior
lines(theta, 44*dbinom(24, size = 40, p = theta), lty = 3) # likelihood (basically what the data gives us)

# probability that theta is > 1/4
1- pbeta(.25, 32, 20)
# Probability that theta is > 1/2
1 - pbeta(.5, 32, 20)
# probability that theta is > 0.8
1- pbeta(.8, 32, 20)


# What is a 95% credible interval?
qbeta(.025, 32, 20)
qbeta(.975, 32, 20)


#### NOTICE
# there is overlap between the two students


# 7. What is the posterior probability that theta2>theta2

# This is hard to do in closed form, so we will estimate by simulation
# Draw 1000 samples of each student and see how often we observe
# theta1 > theta2

theta1 = rbeta(1000, 41, 11) # we sample from each posterior distribution
theta2 = rbeta(1000, 32, 20)
mean(theta1>theta2) # this is the empirical probability that theta1 > theta2


# Note for other distributions:
# dgamma, pgamma, qgamma, rgamma
# dnorm, pnorm, qnorm, rnorm

