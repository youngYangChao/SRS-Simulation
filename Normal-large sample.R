# Set up
library(logspline)

################# Large sample size
# Generate 1000 data from normal distribution
data_norm = rnorm(1000, 5, 1)

# Construct the correct density of the normal distribution
x = seq(2, 9, length = 1000)
y = dnorm(x, 5, 1)

# Results from logspline density estimation
fit = logspline(data_norm)
# Here (-250:250)/10 means the vector of quantiles
dd = dlogspline((-250:250)/10, fit)

# Draw the correct curve and curves using the three methods mentioned in report
# Histogram
hist(data_norm, col = "white", xlim = c(2, 9), ylim = c(0, 0.5), xlab = "x", cex.main = 1.0,
     main = "Normal distribution: Density estimation and real density", freq = F)
# Correct density curve
lines(x, y, lwd = 2, xlim = c(2, 9), ylim = c(0, 0.5), col = "red", lty = 2)
# Logspline density estimation
lines((-250:250)/10, dd, xlim = c(2, 9), ylim = c(0, 0.5), col = "green", type = "l")
# Kernel density estimation
lines(density(data_norm, kernel = "gaussian", bw = "nrd0"), col = "blue")
text(2.5, 0.4, "n=1000")
legend(7,0.4, legend = c("kernel", "logspline","real density"),
       col = c("blue", "green","red"), lty = c(1, 1, 2), cex = 1.0)
