# Set up
install.packages("sfsmisc")
library(sfsmisc)
library(logspline)
library(histogram)
library(EnvStats)

################################### Kernel
# n=250: ISE=0.00462273
# n=500: ISE=0.00297481
# n=1000: ISE=0.001807604
################################### Histogram
# n=250: ISE=0.01119321
# n=500: ISE=0.006458962
# n=1000: ISE=0.003959865
################################### Logspline
# n=250: ISE=0.005266927
# n=500: ISE=0.002878334
# n=1000: ISE=0.001617657

################################################################## Sample size 250
set.seed(33)
################################### Kernel
# Set the sample size
n1 = 250
# Set a empty container
ISE1_n1 = numeric()
# Kernel density estimation
for(i in 1:1000){
  # Generate n1=250 data from mixture of normal distributions
  data_mixturenormal = rnormMix(n1, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  
  # Kernel density estimation
  kernel_estimation_norm = density(data_mixturenormal, kernel = "gaussian")
  
  # Kernel density estimation ISE 
  ISE1_n1[i] = integrate.xy(x = kernel_estimation_norm$x, (kernel_estimation_norm$y - dnormMix(kernel_estimation_norm$x, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5))^2)
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
  # Generate n1=250 data from mixture of normal distributions
  data_mixturenormal = rnormMix(n1, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  
  # Histogram
  histogram_estimation_norm = histogram(data_mixturenormal, type="regular", penalty="cv", freq = FALSE, 
                                        control = list(cvformula = 1), plot = FALSE)
  #ISE-hist
  ordered_x = sort(data_mixturenormal)
  func = dnormMix(ordered_x, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  func_est = numeric()
  for(j in 1:length(histogram_estimation_norm$counts)){
    func_est= c(func_est, rep(histogram_estimation_norm$density[j], each = histogram_estimation_norm$counts[j]))
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
  data_mixturenormal = rnormMix(n1, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  
  # Logspline 
  logspline_estimation_norm = logspline(data_mixturenormal)
  
  # Logspline density estimation ISE 
  func_est = dlogspline(data_mixturenormal, logspline_estimation_norm)
  func = dnormMix(data_mixturenormal, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  ISE3_n1[i] = integrate.xy(data_mixturenormal, (func_est - func)^2)  
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
  # Generate n1=250 data from mixture of normal distributions
  data_mixturenormal = rnormMix(n2, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  
  # Kernel density estimation
  kernel_estimation_norm = density(data_mixturenormal, kernel = "gaussian")
  
  # Kernel density estimation ISE 
  ISE1_n2[i] = integrate.xy(x = kernel_estimation_norm$x, (kernel_estimation_norm$y - dnormMix(kernel_estimation_norm$x, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5))^2)
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
  # Generate n1=250 data from mixture of normal distributions
  data_mixturenormal = rnormMix(n2, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  
  # Histogram
  histogram_estimation_norm = histogram(data_mixturenormal, type="regular", penalty="cv", freq = FALSE, 
                                        control = list(cvformula = 1), plot = FALSE)
  #ISE-hist
  ordered_x = sort(data_mixturenormal)
  func = dnormMix(ordered_x, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  func_est = numeric()
  for(j in 1:length(histogram_estimation_norm$counts)){
    func_est= c(func_est, rep(histogram_estimation_norm$density[j], each = histogram_estimation_norm$counts[j]))
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
  data_mixturenormal = rnormMix(n2, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  
  # Logspline 
  logspline_estimation_norm = logspline(data_mixturenormal)
  
  # Logspline density estimation ISE 
  func_est = dlogspline(data_mixturenormal, logspline_estimation_norm)
  func = dnormMix(data_mixturenormal, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  ISE3_n2[i] = integrate.xy(data_mixturenormal, (func_est - func)^2)  
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
  # Generate n1=250 data from mixture of normal distributions
  data_mixturenormal = rnormMix(n3, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  
  # Kernel density estimation
  kernel_estimation_norm = density(data_mixturenormal, kernel = "gaussian")
  
  # Kernel density estimation ISE 
  ISE1_n3[i] = integrate.xy(x = kernel_estimation_norm$x, (kernel_estimation_norm$y - dnormMix(kernel_estimation_norm$x, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5))^2)
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
  # Generate n1=250 data from mixture of normal distributions
  data_mixturenormal = rnormMix(n3, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  
  # Histogram
  histogram_estimation_norm = histogram(data_mixturenormal, type="regular", penalty="cv", freq = FALSE, 
                                        control = list(cvformula = 1), plot = FALSE)
  #ISE-hist
  ordered_x = sort(data_mixturenormal)
  func = dnormMix(ordered_x, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  func_est = numeric()
  for(j in 1:length(histogram_estimation_norm$counts)){
    func_est= c(func_est, rep(histogram_estimation_norm$density[j], each = histogram_estimation_norm$counts[j]))
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
  data_mixturenormal = rnormMix(n3, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  
  # Logspline 
  logspline_estimation_norm = logspline(data_mixturenormal)
  
  # Logspline density estimation ISE 
  func_est = dlogspline(data_mixturenormal, logspline_estimation_norm)
  func = dnormMix(data_mixturenormal, mean1 = 5, sd1 = 1, mean2 = 9, sd2 = 1, p.mix = 0.5)
  ISE3_n3[i] = integrate.xy(data_mixturenormal, (func_est - func)^2)  
}

# Calculate the mean of the ISE
mean(ISE3_n3)

################################### Draw a boxplot for normal distribution
df = data.frame(ISE1_n1,ISE3_n1,ISE1_n2,ISE3_n2,ISE1_n3,ISE3_n3)
boxplot(df, col=c("skyblue1","pink1","skyblue1","pink1","skyblue1","pink1"), 
        names = c("n=250","n=250","n=500","n=500","n=1000","n=1000"))
legend("topright", legend = c("kernel", "logspline"),
       col = c("skyblue1", "pink1"), lty = c(1, 1), cex = 1.0)
title("ISE boxplot for Mixture of normal distribution", cex.main = 1.0)
