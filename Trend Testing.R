# Kendall rank correlation test + Spearman rank correlation test
# Yawen Zhang
# Feb.14, 2017

#######################################################################
# read the dataset
test_data = read.table("/Users/yawen/Desktop/Courses/APPM 5540 Time series analysis/HW4/17-data-1-4-3.txt")
N = nrow(test_data)
test_data$index = c(1:N)

#######################################################################
# Kendall rank correlation test
# initialize two count, and calculate K
count_less_than = 0
count_equal_to = 0
for(i in 1:(N-1))
{
  for(j in (i+1):N)
  {
    if(test_data$V1[i] < test_data$V1[j])
    {
      count_less_than = count_less_than+1
    }
    if(test_data$V1[i] == test_data$V1[j])
    {
      count_equal_to = count_equal_to+1
    }
  }
}
count_equal_to = count_equal_to/2
print(count_less_than)
print(count_equal_to)
K = count_less_than + count_equal_to
print(K)
# calculate the Kendall coefficient
T_Kendall = 4*K/N/(N-1)-1
print(T_Kendall)
T_variance = 2*(2*N+5)/9/N/(N-1)
print(T_variance)
T_coefficient = T_Kendall/sqrt(T_variance)
print(T_coefficient)

#######################################################################
# Spearman rank correlation test
# calculate rank for each instance
test_data$rank = rank(test_data$V1)
print(test_data$rank)
# calculate S
S_Spearman = 0
for(i in 1:N)
{
  S_Spearman = S_Spearman+(test_data$rank[i]-i)*(test_data$rank[i]-i) 
}
S_Spearman = 1-6*S_Spearman/N/(N*N-1)
print(S_Spearman)
# caculate the Spearman coefficient
S_variance = 1/N
print(S_variance)
S_coefficient = S_Spearman/sqrt(S_variance)
print(S_coefficient)

#######################################################################
# plotting the original data
plot(test_data$V1, xlab = "index", ylab = "value", type = "l", col = "blue")
