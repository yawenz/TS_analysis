# Estimate the sample mean, variance, aoto correlation
# Yawen Zhang
# March 15th, 2017

input_data = read.table("/Users/yawen/Desktop/Courses/APPM 5540 Time series analysis/HW8/17-data-3-2-1.txt")
N = length(input_data$V1)
input_mean = mean(input_data$V1)
input_var = sum((input_data$V1-input_mean)^2)/(N-1) 
R = numeric(10)
auto_correlation = numeric(10)
R0 = 0
for(i in c(1:N))
{R0 = R0 + (input_data$V1[i] - input_mean)^2}
R0 = R0/N
for(k in c(1:10))
{
  R[k] = 0
  for(i in c(1:(N-k)))
  {R[k] = R[k] + (input_data$V1[i] - input_mean)*(input_data$V1[i+k] - input_mean)}
  R[k] = R[k]/N
}
auto_correlation = R/R0
a = auto_correlation
print((a[2]-a[1]^2)/(1-a[1]^2))
print((a[3]+a[1]*a[2]^2+a[1]^3-2*a[1]*a[2]-(a[1]^2)*a[3])/(1+2*a[2]*a[1]^2-a[2]^2-2*a[1]^2))