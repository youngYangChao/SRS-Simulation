# Set up
install.packages("EnvStats")
library(EnvStats)
library(logspline)

################# Large sample size
# Generate 1000 data from mixture of normal distributions
mixnorm_data = rnormMix(1000, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)

# Construct the correct density of the normal distribution
x = seq(0, 30, length = 1000)
y = 0.5*dnorm(x, 5, 1) + 0.5*dnorm(x, 9, 1) 

# Results from logspline density estimation
fit = logspline(mixnorm_data)
# Here (-250:250)/10 means the vector of quantiles
dd = dlogspline((-250:250)/10, fit)

# Draw the correct curve and curves using the three methods mentioned in report
# Histogram
hist(mixnorm_data, col = "white", xlim = c(1, 13), ylim = c(0, 0.3), xlab = "x", cex.main = 1.0,
     main = "Mixture of normal distribution: Density estimation and real density", freq = F)
# Correct density curve
lines(x, y, lwd = 2, xlim = c(1, 13), ylim = c(0, 0.3), col = "red", lty = 2)
# Logspline density estimation
lines((-250:250)/10, dd, xlim = c(1, 13), ylim = c(0, 0.3), col = "green", type = "l")
# Kernel density estimation
lines(density(mixnorm_data, kernel = "gaussian", bw = "nrd0"), col = "blue")
# Add comment
text(2, 0.25, "n=1000")
# Add the legend
legend(10, 0.3, legend = c("kernel", "logspline","real density"),
       col=c("blue", "green", "red"), lty = c(1, 1, 2), cex = 1.0)