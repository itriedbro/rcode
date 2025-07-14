library(pracma)


# (i) Define the joint probability density function f(x, y)
f_xy <- function(x, y) {
  # Vectorized condition check using element-wise '&'
  cond <- (x >= 0 & x <= 1 & y >= 0 & y <= 1)
  # Use ifelse to return correct values for each element in the vectors
  return(ifelse(cond, 2 * (2 * x + 3 * y) / 5, 0))
}


# (i) Check if it's a valid joint density function (total integral must be 1)
joint_density_integral <- integral2(function(x, y) f_xy(x, y), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
cat("Total integral of joint density function (should be 1):", joint_density_integral$Q, "\n")



# (ii) Find the marginal distribution g(x) at x = 1
# g(x) = ∫ f(x, y) dy from 0 to 1
g_x_at_1 <- integral(function(y) f_xy(1, y), 0, 1)
cat("Marginal distribution g(x) at x = 1:", g_x_at_1, "\n")



# (iii) Find the marginal distribution h(y) at y = 0
# h(y) = ∫ f(x, y) dx from 0 to 1
h_y_at_0 <- integral(function(x) f_xy(x, 0), 0, 1)
cat("Marginal distribution h(y) at y = 0:", h_y_at_0, "\n")




# (iv) Find the expected value of g(x, y) = xy
# E[xy] = ∫∫ x * y * f(x, y) dx dy
expected_value_xy <- integral2(function(x, y) x * y * f_xy(x, y), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
cat("Expected value of g(x, y) = xy:", expected_value_xy$Q, "\n")





#Q2


# (i) Define the joint probability mass function in matrix form
x_vals <- 0:3  # possible values of X
y_vals <- 0:2  # possible values of Y
joint_pmf <- outer(x_vals, y_vals, function(x, y) (x + y) / 30)  # joint PMF as a matrix
cat("Joint Probability Mass Function (PMF):\n")
print(joint_pmf)



# (ii) Check if it is a valid joint mass function (sum should be 1)
total_sum <- sum(joint_pmf)
cat("\nSum of all probabilities (should be 1):", total_sum, "\n")




# (iii) Find marginal distribution g(x) for x = 0, 1, 2, 3
g_x <- apply(joint_pmf, 1, sum)  # sum along rows (y-values)
cat("\nMarginal distribution g(x):\n")
print(g_x)



# (iv) Find marginal distribution h(y) for y = 0, 1, 2
h_y <- apply(joint_pmf, 2, sum)  # sum along columns (x-values)
cat("\nMarginal distribution h(y):\n")
print(h_y)




# (v) Find conditional probability P(X = 0 | Y = 1)
# P(X = 0 | Y = 1) = f(0, 1) / h(1)
conditional_prob_x0_given_y1 <- joint_pmf[1, 2] / h_y[2]
cat("\nConditional probability P(X = 0 | Y = 1):", conditional_prob_x0_given_y1, "\n")



# (vi) Compute E(X), E(Y), E(XY), Var(X), Var(Y), Cov(X, Y) and correlation coefficient

# E(X) = sum over x of x * g(x)
E_X <- sum(x_vals * g_x)
cat("\nE(X):", E_X, "\n")

# E(Y) = sum over y of y * h(y)
E_Y <- sum(y_vals * h_y)
cat("E(Y):", E_Y, "\n")

# E(XY) = sum over x and y of x * y * f(x, y)
E_XY <- sum(outer(x_vals, y_vals, "*") * joint_pmf)
cat("E(XY):", E_XY, "\n")

# Var(X) = E(X^2) - (E(X))^2
E_X2 <- sum(x_vals^2 * g_x)
Var_X <- E_X2 - E_X^2
cat("Var(X):", Var_X, "\n")

# Var(Y) = E(Y^2) - (E(Y))^2
E_Y2 <- sum(y_vals^2 * h_y)
Var_Y <- E_Y2 - E_Y^2
cat("Var(Y):", Var_Y, "\n")

# Cov(X, Y) = E(XY) - E(X) * E(Y)
Cov_XY <- E_XY - (E_X * E_Y)
cat("Cov(X, Y):", Cov_XY, "\n")

# Correlation coefficient = Cov(X, Y) / (sqrt(Var(X)) * sqrt(Var(Y)))
correlation_coefficient <- Cov_XY / (sqrt(Var_X) * sqrt(Var_Y))
cat("Correlation coefficient:", correlation_coefficient, "\n")




















