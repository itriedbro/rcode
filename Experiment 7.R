n <- 100
df <- n - 1
t_values <- rt(n, df)
hist(t_values, main = "Histogram of t-distribution (n=100, df=99)", xlab = "t-values", col = "lightblue", border = "black")


n <- 100

chi_square_df2 <- rchisq(n, df = 2)
chi_square_df10 <- rchisq(n, df = 10)
chi_square_df25 <- rchisq(n, df = 25)

par(mfrow = c(1, 3))  # Arrange plots in a 1x3 grid
hist(chi_square_df2, main = "Chi-square (df=2)", xlab = "Values", col = "lightgreen", border = "black")
hist(chi_square_df10, main = "Chi-square (df=10)", xlab = "Values", col = "lightcoral", border = "black")
hist(chi_square_df25, main = "Chi-square (df=25)", xlab = "Values", col = "lightblue", border = "black")
par(mfrow = c(1, 1))  # Reset to default layout



x <- seq(-6, 6, length.out = 100)

density_df1 <- dt(x, df = 1)
density_df4 <- dt(x, df = 4)
density_df10 <- dt(x, df = 10)
density_df30 <- dt(x, df = 30)

plot(x, density_df30, type = "l", col = "blue", lwd = 2, ylim = c(0, 0.4),
     main = "t-distribution Density Comparison", ylab = "Density", xlab = "x")
lines(x, density_df1, col = "red", lwd = 2)
lines(x, density_df4, col = "green", lwd = 2)
lines(x, density_df10, col = "purple", lwd = 2)
# Add legend
legend("topright", legend = c("df=1", "df=4", "df=10", "df=30"), col = c("red", "green", "purple", "blue"), lwd = 2)
