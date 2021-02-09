# Set up
install.packages("sfsmisc")
library(sfsmisc)
library(logspline)
library(histogram)
library(EnvStats)

################################### Kernel
# n=250: ISE=0.003212807
# n=500: ISE=0.001881695
# n=1000: ISE=0.001072509
################################### Histogram
# n=250: ISE=0.01623037
# n=500: ISE=0.009146174
# n=1000: ISE=0.005477232
################################### Logspline
# n=250: ISE=0.006789065
# n=500: ISE=0.003297742
# n=1000: ISE=0.001477493

################################################################## Sample size 250
set.seed(33)
################################### Kernel
# Set the sample size
n1 = 250
# Set a empty container
ISE1_n1 = numeric()
# Kernel density estimation
for(i in 1:1000){
  # Generate n1=250 data from normal distribution
  data_norm = rnorm(n1, 5, 1)
  
  # Kernel density estimation
  kernel_estimation_norm = density(data_norm, kernel = "gaussian")
  
  # Kernel density estimation ISE 
  ISE1_n1[i] = integrate.xy(x = kernel_estimation_norm$x, (kernel_estimation_norm$y - dnorm(kernel_estimation_norm$x, 5, 1))^2)
}

# Calculate the mean of the ISE
mean(ISE1_n1)



################################### Histogram
# Set the sample size
n1 = 250
# Set a empty container
ISE2_n1 = numeric()
# Histogram density estimation
for(i in 1:1000){
  # Generate n1=250 data from normal distribution
  data_norm = rnorm(n1, 5, 1)
  
  # Histogram
  histogram_estimation_norm = histogram(data_norm, type="regular", penalty="cv", freq = FALSE, 
                                        control = list(cvformula = 1), plot = FALSE)
  
  # Histogram ISE 
  # Sort the data from small to large
  ordered_x = sort(data_norm)
  func = dnorm(ordered_x, 5, 1)
  func_est = numeric()
  for(j in 1:length(histogram_estimation_norm$counts)){
    func_est = c(func_est, rep(histogram_estimation_norm$density[j], each = histogram_estimation_norm$counts[j]))
  }
  ISE2_n1[i] = integrate.xy(ordered_x, (func_est - func)^2)
}

# Calculate the mean of the ISE
mean(ISE2_n1)



################################### Logspline
# Set the sample size
n1 = 250
# Set a empty container
ISE3_n1 = numeric()
# Logspline density estimation
for(i in 1:1000){ 
  # Generate n1=250 data from normal distribution
  data_norm = rnorm(n1, 5, 1)
  
  # Logspline 
  logspline_estimation_norm = logspline(data_norm)
  
  # Logspline density estimation ISE 
  y = dlogspline(data_norm, logspline_estimation_norm)
  ISE3_n1[i] = integrate.xy(data_norm, (y - dnorm(data_norm, 5, 1))^2)  
}

# Calculate the mean of the ISE
mean(ISE3_n1)



################################################################## Sample size 500
################################### Kernel
# Set the sample size
n2 = 500
# Set a empty container
ISE1_n2 = numeric()
# Kernel density estimation
for(i in 1:1000){
  # Generate n1=500 data from normal distribution
  data_norm = rnorm(n2, 5, 1)
  
  # Kernel density estimation
  kernel_estimation_norm = density(data_norm, kernel = "gaussian")
  
  # Kernel density estimation ISE 
  ISE1_n2[i] = integrate.xy(x = kernel_estimation_norm$x, (kernel_estimation_norm$y - dnorm(kernel_estimation_norm$x, 5, 1))^2)
}

# Calculate the mean of the ISE
mean(ISE1_n2)



################################### Histogram
# Set the sample size
n2 = 500
# Set a empty container
ISE2_n2 = numeric()
# Histogram density estimation
for(i in 1:1000){
  # Generate n1=250 data from normal distribution
  data_norm = rnorm(n2, 5, 1)
  
  # Histogram
  histogram_estimation_norm = histogram(data_norm, type="regular", penalty="cv", freq = FALSE, 
                                        control = list(cvformula = 1), plot = FALSE)
  
  # Histogram ISE 
  # Sort the data from small to large
  ordered_x = sort(data_norm)
  func = dnorm(ordered_x, 5, 1)
  func_est = numeric()
  for(j in 1:length(histogram_estimation_norm$counts)){
    func_est = c(func_est, rep(histogram_estimation_norm$density[j], each = histogram_estimation_norm$counts[j]))
  }
  ISE2_n2[i] = integrate.xy(ordered_x, (func_est - func)^2)
}

# Calculate the mean of the ISE
mean(ISE2_n2)



################################### Logspline
# Set the sample size
n2 = 500
# Set a empty container
ISE3_n2 = numeric()
# Logspline density estimation
for(i in 1:1000){ 
  # Generate n1=250 data from normal distribution
  data_norm = rnorm(n2, 5, 1)
  
  # Logspline 
  logspline_estimation_norm = logspline(data_norm)
  
  # Logspline density estimation ISE 
  y = dlogspline(data_norm, logspline_estimation_norm)
  ISE3_n2[i] = integrate.xy(data_norm, (y - dnorm(data_norm, 5, 1))^2)  
}

# Calculate the mean of the ISE
mean(ISE3_n2)



################################################################## Sample size 1000
################################### Kernel
# Set the sample size
n3 = 1000
# Set a empty container
ISE1_n3 = numeric()
# Kernel density estimation
for(i in 1:1000){
  # Generate n1=500 data from normal distribution
  data_norm = rnorm(n3, 5, 1)
  
  # Kernel density estimation
  kernel_estimation_norm = density(data_norm, kernel = "gaussian")
  
  # Kernel density estimation ISE 
  ISE1_n3[i] = integrate.xy(x = kernel_estimation_norm$x, (kernel_estimation_norm$y - dnorm(kernel_estimation_norm$x, 5, 1))^2)
}

# Calculate the mean of the ISE
mean(ISE1_n3)



################################### Histogram
# Set the sample size
n3 = 1000
# Set a empty container
ISE2_n3 = numeric()
# Histogram density estimation
for(i in 1:1000){
  # Generate n1=250 data from normal distribution
  data_norm = rnorm(n3, 5, 1)
  
  # Histogram
  histogram_estimation_norm = histogram(data_norm, type="regular", penalty="cv", freq = FALSE, 
                                        control = list(cvformula = 1), plot = FALSE)
  
  # Histogram ISE 
  # Sort the data from small to large
  ordered_x = sort(data_norm)
  func = dnorm(ordered_x, 5, 1)
  func_est = numeric()
  for(j in 1:length(histogram_estimation_norm$counts)){
    func_est = c(func_est, rep(histogram_estimation_norm$density[j], each = histogram_estimation_norm$counts[j]))
  }
  ISE2_n3[i] = integrate.xy(ordered_x, (func_est - func)^2)
}

# Calculate the mean of the ISE
mean(ISE2_n3)



################################### Logspline
# Set the sample size
n3 = 1000
# Set a empty container
ISE3_n3 = numeric()
# Logspline density estimation
for(i in 1:1000){ 
  # Generate n1=250 data from normal distribution
  data_norm = rnorm(n3, 5, 1)
  
  # Logspline 
  logspline_estimation_norm = logspline(data_norm)
  
  # Logspline density estimation ISE 
  y = dlogspline(data_norm, logspline_estimation_norm)
  ISE3_n3[i] = integrate.xy(data_norm, (y - dnorm(data_norm, 5, 1))^2)  
}

# Calculate the mean of the ISE
mean(ISE3_n3)


################################### Draw a boxplot for normal distribution
df = data.frame(ISE1_n1,ISE3_n1,ISE1_n2,ISE3_n2,ISE1_n3,ISE3_n3)
boxplot(df, col=c("skyblue1","pink1","skyblue1","pink1","skyblue1","pink1"), 
        names = c("n=250","n=250","n=500","n=500","n=1000","n=1000"))
legend("topright", legend = c("kernel", "logspline"),
       col = c("skyblue1", "pink1"), lty = c(1, 1), cex = 1.0)
title("ISE boxplot for N(5, 1)", cex.main = 1.0)