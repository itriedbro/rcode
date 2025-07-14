x = c(0,1,2,3,4)
p = c(0.41, 0.37, 0.16, 0.05, 0.01)

exp <- sum(x*p)
exp

exp2 <- weighted.mean(x,p)
exp2





# Function for the probability density function
  f_t <- function(t) {
    0.1 * exp(-0.1 * t)
  }

# Integrating to find expected value
expected_value_t <- integrate(function(t) t * f_t(t), lower = 0, upper = Inf)$value
expected_value_t




# Data
x <- c(0, 1, 2, 3)  # Number of copies sold
p_x <- c(0.1, 0.2, 0.2, 0.5)  # Probability distribution of X

# Net revenue calculation after subtracting initial investment of $18
# Revenue values considering both sales and returns
net_revenue <- c(-12, -2, 8, 18)  # Net revenue values for X = 0, 1, 2, 3

# Calculate expected net revenue
expected_net_revenue <- sum(net_revenue * p_x)
expected_net_revenue





# Define the PDF function
f_x <- function(x) {
  0.5 * exp(-abs(x))
}

# First moment (Mean)
first_moment <- integrate(function(x) x * f_x(x), lower = 1, upper = 10)$value

# Second moment
second_moment <- integrate(function(x) x^2 * f_x(x), lower = 1, upper = 10)$value

# Calculate Variance
mean_x <- first_moment
variance_x <- second_moment - mean_x^2

first_moment
second_moment
variance_x





# Define the geometric distribution for X
f_x <- function(x) {
  (3 / 4) * (1 / 4)^(x - 1)
}

# Function to calculate the probability of Y = X^2
prob_y <- function(x) {
  f_x(x)
}

# Calculate probability of Y for X = 3
x_val <- 3
y_val <- x_val^2
prob_y_x3 <- prob_y(x_val)

# Expected value and variance of Y for X = 1 to 5
x_values <- 1:5
y_values <- x_values^2

# Expected value of Y
expected_value_y <- sum(y_values * sapply(x_values, prob_y))

# Second moment of Y
second_moment_y <- sum(y_values^2 * sapply(x_values, prob_y))

# Variance of Y
variance_y <- second_moment_y - expected_value_y^2

list(
  probability_y_x3 = prob_y_x3,
  expected_value_y = expected_value_y,
  variance_y = variance_y
)










