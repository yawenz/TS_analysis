# Exponential smoothing of a dataset
# Yawen Zhang
# Feb.14, 2017

#######################################################################
# read the dataset
test_data = read.table("/Users/yawen/Desktop/Courses/APPM 5540 Time series analysis/HW4/17-data-1-3-4.txt")
N = nrow(test_data)
test_data$index = c(1:N)
alpha = 0.05

#######################################################################
# applying zero order exponential smoothing with alpha = 0.05
test_data$prediction_zero = NA
test_data$a_zero = NA
# initialization
test_data$a_zero[1] = test_data$V1[1]
# one step ahead prediction
test_data$prediction_zero[1] = test_data$a_zero[1]
for(i in 2:N)
{
  error = test_data$V1[i] - test_data$prediction_zero[i-1]
  test_data$a_zero[i] = test_data$a_zero[i-1] + alpha*error
  test_data$prediction_zero[i] = test_data$a_zero[i]
}
#######################################################################
# applying first order exponential smoothing with alpha = 0.05
test_data$prediction_first = NA
test_data$a_first = NA
test_data$b_first = NA
# initialization
test_data$a_first[1] = test_data$V1[1]
test_data$b_first[1] = 0
# one step ahead prediction
test_data$prediction_first[1] = test_data$a_first[1]
for(i in 2:N)
{
  error = test_data$V1[i] - test_data$prediction_first[i-1]
  test_data$a_first[i] = test_data$a_first[i-1] + test_data$b_first[i-1] + (2*alpha-alpha*alpha)*error
  test_data$b_first[i] = test_data$b_first[i-1] + alpha*alpha*error
  test_data$prediction_first[i] = test_data$a_first[i] + test_data$b_first[i]
}
#######################################################################
# applying second order exponential smoothing with alpha = 0.05
test_data$prediction_second = NA
test_data$a_second = NA
test_data$b_second = NA
test_data$c_second = NA
# initialization
test_data$a_second[1] = test_data$V1[1]
test_data$b_second[1] = 0
test_data$c_second[1] = 0
# one step ahead prediction
test_data$prediction_second[1] = test_data$a_second[1]
for(i in 2:N)
{
  error = test_data$V1[i] - test_data$prediction_second[i-1]
  test_data$a_second[i] = test_data$a_second[i-1] + test_data$b_second[i-1] + test_data$c_second[i-1] + alpha*(3-3*alpha+alpha*alpha)*error
  test_data$b_second[i] = test_data$b_second[i-1] + 2*test_data$c_second[i-1] + 3*alpha*alpha*(1-alpha/2)*error
  test_data$c_second[i] = test_data$c_second[i-1] + alpha*alpha*alpha/2*error
  test_data$prediction_second[i] = test_data$a_second[i] + test_data$b_second[i] + test_data$c_second[i]
}

#######################################################################
# plotting the results

# plotting the original data
plot(test_data$V1, xlab = "index", ylab = "value", type = "l", col = "blue", ylim = range(1500:2500))
# ploting zero order exponential smoothing
lines(x = test_data$index, y = test_data$prediction_zero, col="orange")
# ploting first order exponential smoothing
lines(x = test_data$index, y = test_data$prediction_first, col="green")
# ploting second order exponential smoothing
lines(x = test_data$index, y = test_data$prediction_second, col="red")
# add legend
legend("bottomleft", c("orginal data","zero order","first order","second order"), lty=c(1,1,1,1), lwd=c(2.5,2.5,2.5,2.5), bty="n", col=c("blue","orange","green","red")) 
