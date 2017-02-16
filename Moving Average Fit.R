# Moving Average Fit of a dataset
# Yawen Zhang
# Feb.13, 2017

#######################################################################

# applying moving average of 1st order to a dataset
# read the dataset
test_data = read.table("/Users/yawen/Desktop/Courses/APPM 5540 Time series analysis/HW4/17-data-1-2-5.txt")
N = nrow(test_data)
test_data$index = c(1:N)
# create estimation
test_data$estimation = 0

# window size
l = 15
# fit a trend for 1...l and N-l+1...N, also moving average for l+1...N-l
for(i in 1:l)
{
  test_data[i,3] = mean(test_data[1:(i+l),1]) 
  print("####")
  print(test_data[1:(i+l),1])
  print(test_data[i,3])
}
for(i in (l+1):(N-l))
{
  test_data[i,3] = mean(test_data[(i-l):(i+l),1])
  print("####")
  print(test_data[(i-l):(i+l),1])
  print(test_data[i,3])
}
for(i in (N-l+1):N)
{
  test_data[i,3] = mean(test_data[(i-l):N,1])
}

# 53X smoothing
test_data$Yt = NA
test_data$Zt = NA
test_data$Wt = NA
# calculate Yt
for(k in 1:(N-4))
{
  test_data[(k+2),4] = median(test_data[k:(k+4),1])
}
# calculate Zt
for(m in 3:(N-4))
{
  test_data[(m+1),5] = median(test_data[m:(m+2),4])
}
# calculate Wt
for(n in 4:(N-5))
{
  test_data[(n+1),6] = 0.25*test_data[n,5]+0.5*test_data[(n+1),5]+0.25*test_data[(n+2),5]
}
# caculate moving average based on 53X smoothing
test_data$estimation_53X = NA
# fit a trend for 1...l and N-l+1...N, also moving average for l+1...N-l
for(i in 5:(4+l))
{
  test_data[i,7] = mean(test_data[5:(i+l),6]) 
}
for(i in (5+l):(N-l-4))
{
  test_data[i,7] = mean(test_data[(i-l):(i+l),6])
}
for(i in (N-l-3):(N-4))
{
  test_data[i,7] = mean(test_data[(i-l):(N-4),6])
}

# plotting the original data
plot(test_data[,1], xlab = "index", ylab = "value", type = "l", col = "blue")
# ploting the moving average result
lines(x = test_data[,2], y = test_data[,3], col="orange")
# ploting the 53X smoothing + moving average result
lines(x = test_data[,2], y = test_data[,7], col="green")
# add legend
legend(42, 52, c("orginal data","1st order moving average","53X + moving average"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), , bty = "n", col=c("blue","orange","green")) 

########################################################################

# applying moving average of 2st order to a dataset
# read the dataset
test_data = read.table("/Users/yawen/Desktop/Courses/APPM 5540 Time series analysis/HW4/17-data-1-2-6.txt")
N = nrow(test_data)
test_data$index = c(1:N)
# create estimation
test_data$estimation = 0

# window size
l = 25
# fit a trend for 1...l and N-l+1...N, also moving average for l+1...N-l
# calculate I2 and I4
I2 = l*(l+1)*(2*l+1)/3
I4 = l*(l+1)*(2*l+1)*(3*l*l+3*l-1)/15

for(i in 1:l)
{
  sum_data = 0
  for(j in (-l):l)
  {
    if((i+j)>=1)
    {
      sum_data = sum_data+(1-j*j*I2/I4)*test_data[(i+j),1]
    }
  }
  test_data[i,3] = sum_data/(2*l+1-I2*I2/I4)
}
for(i in (l+1):(N-l))
{
  sum_data = 0
  for(j in (-l):l)
  {
    sum_data = sum_data+(1-j*j*I2/I4)*test_data[(i+j),1]
  }
  test_data[i,3] = sum_data/(2*l+1-I2*I2/I4)
}
for(i in (N-l+1):N)
{
  sum_data = 0
  for(j in (-l):l)
  {
    if((i+j)<=N)
    {
      sum_data = sum_data+(1-j*j*I2/I4)*test_data[(i+j),1]
    }
  }
  test_data[i,3] = sum_data/(2*l+1-I2*I2/I4)
}

# 53X smoothing
test_data$Yt = NA
test_data$Zt = NA
test_data$Wt = NA
# calculate Yt
for(k in 1:(N-4))
{
  test_data[(k+2),4] = median(test_data[k:(k+4),1])
}
# calculate Zt
for(m in 3:(N-4))
{
  test_data[(m+1),5] = median(test_data[m:(m+2),4])
}
# calculate Wt
for(n in 4:(N-5))
{
  test_data[(n+1),6] = 0.25*test_data[n,5]+0.5*test_data[(n+1),5]+0.25*test_data[(n+2),5]
}
# caculate moving average based on 53X smoothing
test_data$estimation_53X = NA
# 2nd order moving average again
for(i in 5:(l+4))
{
  sum_data = 0
  for(j in (-l):l)
  {
    if((i+j)>=5)
    {
      sum_data = sum_data+(1-j*j*I2/I4)*test_data[(i+j),6]
    }
  }
  test_data[i,7] = sum_data/(2*l+1-I2*I2/I4)
}
for(i in (l+5):(N-l-4))
{
  sum_data = 0
  for(j in (-l):l)
  {
    sum_data = sum_data+(1-j*j*I2/I4)*test_data[(i+j),6]
  }
  test_data[i,7] = sum_data/(2*l+1-I2*I2/I4)
}
for(i in (N-l-3):(N-4))
{
  sum_data = 0
  for(j in (-l):l)
  {
    if((i+j)<=(N-4))
    {
      sum_data = sum_data+(1-j*j*I2/I4)*test_data[(i+j),6]
    }
  }
  test_data[i,7] = sum_data/(2*l+1-I2*I2/I4)
}

# plotting the original data
plot(test_data[,1], xlab = "index", ylab = "value", type = "l", col = "blue")
# ploting the moving average result
lines(x = test_data[,2], y = test_data[,3], col="orange")
# ploting the 53X smoothing + moving average result
lines(x = test_data[,2], y = test_data[,7], col="green")
# add legend
legend(170, 117, c("orginal data","2nd order moving average","53X + moving average"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), bty="n", col=c("blue","orange","green")) 





