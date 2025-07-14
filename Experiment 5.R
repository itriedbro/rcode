#Q1: Uniform Distribution (X ~ U(0, 60))
pmore45 <- 1 - punif(45,min = 0, max = 60)
pmore45

pbetw2030 <- punif(30, min = 0, max = 60) - punif(20, min = 0, max = 60)
pbetw2030




#Q2: Exponential Distribution (X ~ Exp(λ))
lambda <- 1/2


# (a) Density function at x = 3
fx3 <- dexp(3,rate = lambda)
fx3


# (b) Plot of the exponential probability distribution for 0 ≤ x ≤ 5
xvals <- seq(0,5,length.out = 100)
yvals <- dexp(xvals,rate = lambda)
plot(xvals, yvals, type = "l", main = "Exponential PDF", xlab = "x", ylab = "Density")


# (c) Probability that repair time takes at most 3 hours
p3 <- pexp(3,rate = lambda)
p3


# (d) Plot of the cumulative exponential probabilities for 0 ≤ x ≤ 5
ycdfvals <- pexp(xvals,rate = lambda)
plot(xvals, ycdfvals, type = "l", main = "Exponential CDF", xlab = "x", ylab = "Cumulative Probability")


# (e) Simulate 1000 exponential distributed random numbers with λ = 1/2 and plot the data
set.seed(123)
simulated <- rexp(1000,rate = lambda)
hist(simulated, breaks = 30, probability = TRUE, main = "Simulated Exponential Data", xlab = "x")



#Q3: Gamma Distribution (X ~ Gamma(α, β))
alpha <- 2
beta <- 1/3  # Note: beta is the rate (inverse of the scale)

# (a) Probability that the lifetime of equipment is:
# (i) 3 units of time
P_X_3 <- dgamma(3, shape = alpha, rate = beta)
P_X_3  

# (ii) At least 1 unit of time
P_at_least_1 <- 1 - pgamma(1, shape = alpha, rate = beta)
P_at_least_1  

# (b) Finding c such that P(X ≤ c) ≥ 0.70
c_value <- qgamma(0.70, shape = alpha, rate = beta)
c_value  








