
###
###fibonacci sequence
###
len <- 15
fibo <- numeric(len)
fibo[2] <- 1
for (i in 3:len) { 
      fibo[i] <- fibo[i-1]+fibo[i-2]
} 


###
###binomial probability (coin toss)
###
#rbinom provides the number of successes of a coin toss, with n = 1 (# runs/observations),
#size = 10 (# of trials/run), and p = 0.5.  Divide by size to obtain probability.
rbinom(1,10,.5)

#1000 runs of 10 trials each, with a fair coin. Calculate mean and sd.
flip <- rbinom(1000, 10,.5)
sdf <- sd(flip)
mf <- mean(flip)


#What is the chance of getting exactly 3 out of 10 successes in a binomial 
#distribution with success probability .7?
dbinom(3, 10, .7)

#What is the chance of getting at least 3 successes? 
sum(dbinom(3:10, 10, .7))

#At most 3? Methods are equivalent.
sum(dbinom(0:3, 10, .7))
pbinom(3, 10, .7)

#Fewer than 3?
pbinom(2, 10, .7)

#Prob of not rolling a 6 in 5 rolls with a fair die? (0 successes in 5 rolls, 
#w prob = 1/6). This is binomial because outcomes are '6' or 'not 6'
dbinom(0, 5, 1/6)

#Roll at least one 1 when rolling 2 fair dice
sum((dbinom(1, 2, 1/6)), (dbinom(2, 2, 1/6)))

#sampling from discrete distribution
#sample(x, size, replace = FALSE, prob = NULL)
#x = options to draw from, size = # of draws
sam1 <- sample(c(1, 2, 3, 4, 5, 6), 100, 
               replace = T, prob = c(.1, .1, .1, .1, .2, .4))
qplot(sam1)
table(sam1)/length(sam1)  #frequency; will approach prob as n increases


#CLT in action. start by creating a skewed dataset, then take progressively
#larger samples from it.
#rnorm(n, mean, sd)
#rchisq(n, mean)
#rexp(n, mean)
set.seed(321) 
xx <- rchisq(100,4)
qplot(xx)
xx <- matrix(rchisq(100000,4),ncol=1000) 
dim(xx) # check the dimensions of the matrix 
xxmean <- apply(xx,2,mean) 
xxsd <- apply(xx,2,sd)
qplot(xxmean)

x=rnorm(500,mean=100,sd=10)