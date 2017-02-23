# Kendall rank correlation test + Spearman rank correlation test
# Yawen Zhang
# Feb.17, 2017

#######################################################################
# read the dataset
test_data = read.table("/Users/yawen/Desktop/Courses/APPM 5540 Time series analysis/HW4/17-1-4-Project-data.txt")
N = nrow(test_data)
test_data$index = c(1:N)

#######################################################################
# Kendall rank correlation test
# create a sequence, with 0.01 increment
s_positive = seq(0.00, 1, by = 0.01)
s_negative = seq(0, -1, by = -0.01)
# using loop to find how big a should be
for(a in s_negative){
  # initialize two count, and calculate K
  count_less_than = 0
  count_equal_to = 0
  test_data$Xt = a*test_data$index+test_data$V1
for(i in 1:(N-1))
{
  for(j in (i+1):N)
  {
    if(test_data$Xt[i] < test_data$Xt[j])
    {
      count_less_than = count_less_than+1
    }
    if(test_data$Xt[i] == test_data$Xt[j])
    {
      count_equal_to = count_equal_to+1
    }
  }
}
count_equal_to = count_equal_to/2
K = count_less_than + count_equal_to
# calculate the Kendall coefficient
T_Kendall = 4*K/N/(N-1)-1
T_variance = 2*(2*N+5)/9/N/(N-1)
T_coefficient = abs(T_Kendall)/sqrt(T_variance)
# print how big should be a, set 5% level of significance
if(T_coefficient >= 1.96) break
}
print("Kendall a:")
print(K)
print(T_Kendall)
print(T_coefficient)
print(a)

#######################################################################
# Spearman rank correlation test
# calculate rank for each instance
for(b in s_negative){
test_data$Yt = b*test_data$index+test_data$V1
test_data$rank = rank(test_data$Yt)
# calculate S
S_Spearman = 0
for(i in 1:N)
{
  S_Spearman = S_Spearman+(test_data$rank[i]-i)*(test_data$rank[i]-i) 
}
S_Spearman = 1-6*S_Spearman/N/(N*N-1)
# caculate the Spearman coefficient
S_variance = 1/N
S_coefficient = abs(S_Spearman)/sqrt(S_variance)
# print how big should be a, set 5% level of significance
if(S_coefficient >= 1.96) break
}
print("Spearman a:")
print(S_Spearman)
print(S_coefficient)
print(b)
#######################################################################
# plotting
plot(test_data$Xt, xlab = "index", ylab = "value", type = "l", col = "blue")
abline(0, a, col = "orange")
