# linear trend of a dataset
# Yawen Zhang
# Feb.09, 2017

# read the dataset
test_data = read.table("/Users/yawen/Desktop/Courses/APPM 5540 Time series analysis/HW3/17-data-I-1-3.txt")
print(test_data)

# set the parameters for linear fitting
N = nrow(test_data)
t_average = (N + 1) / 2
# a_transform estimation
a_transform_estimation = sum(test_data) / N
# b estimation
b_estimation_denominator = 0
b_estimation_numerator = 0
for(i in 1:N)
{
  print(b_estimation_numerator)
  b_estimation_numerator = b_estimation_numerator + (i - t_average) * test_data[i, 1]
  b_estimation_denominator = b_estimation_denominator + (i - t_average) * (i - t_average)
}
b_estimation = b_estimation_numerator / b_estimation_denominator
# a estimation
a_estimation = a_transform_estimation - b_estimation * t_average

# plot fitting result
plot(test_data[,1], xlab = "index", ylab = "value", type = "l", col = "blue", xlim = c(0, 50), ylim = c(0, 30))
abline(a = a_estimation, b = b_estimation, col = "orange")

# testing hypothesis H0: b = 0
# calculate Q
Q = 0
for(i in 1:N)
{
  Q = Q + (test_data[i, 1] - a_estimation - b_estimation * i) * (test_data[i, 1] - a_estimation - b_estimation * i)
}
# noise estimation
noise_variance_estimation = Q / (N - 2)
noise_variance_estimation_sqrt = sqrt(noise_variance_estimation)
#######################################
# t estimation
t_subset = 0
for(i in 1:N)
{
  t_subset = t_subset + (i - t_average) * (i - t_average)
}
t_estimation = b_estimation * sqrt(t_subset) / noise_variance_estimation_sqrt
# t statistics : get from website (http://yuppal.people.ysu.edu/econ_3790/t-table.pdf) N = 41, alpha = 0.05
t_statistics = 2.023

# absolution t_estimation is larger than t_statistics, we reject H0, the trend is statistically significant

# make prediction for next five steps
X = matrix(0, ncol = 1, nrow = 5)
X[1] = a_transform_estimation + b_estimation * (N + 1 - t_average)
X[2] = X[1] + b_estimation
X[3] = X[2] + b_estimation
X[4] = X[3] + b_estimation
X[5] = X[4] + b_estimation

# confidence bounds for prediction
# uppper bound
X_upper_bound = matrix(0, ncol = 1, nrow = 5)
X_upper_bound[1] = X[1] + t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 1 - t_average) * (N + 1 - t_average) / t_subset)
X_upper_bound[2] = X[2] + t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 2 - t_average) * (N + 2 - t_average) / t_subset)
X_upper_bound[3] = X[3] + t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 3 - t_average) * (N + 3 - t_average) / t_subset)
X_upper_bound[4] = X[4] + t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 4 - t_average) * (N + 4 - t_average) / t_subset)
X_upper_bound[5] = X[5] + t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 5 - t_average) * (N + 5 - t_average) / t_subset)
# lower bound
X_lower_bound = matrix(0, ncol = 1, nrow = 5)
X_lower_bound[1] = X[1] - t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 1 - t_average) * (N + 1 - t_average) / t_subset)
X_lower_bound[2] = X[2] - t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 2 - t_average) * (N + 2 - t_average) / t_subset)
X_lower_bound[3] = X[3] - t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 3 - t_average) * (N + 3 - t_average) / t_subset)
X_lower_bound[4] = X[4] - t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 4 - t_average) * (N + 4 - t_average) / t_subset)
X_lower_bound[5] = X[5] - t_statistics * noise_variance_estimation_sqrt * sqrt(1 + 1 / N + (N + 5 - t_average) * (N + 5 - t_average) / t_subset)

# plot the upper and lower bounds
lines(x = c(42, 43, 44, 45, 46), X[1:5,1], col = "black")
lines(x = c(42, 43, 44, 45, 46), X_upper_bound[1:5,1], col = "green")
lines(x = c(42, 43, 44, 45, 46), X_lower_bound[1:5,1], col = "red")



