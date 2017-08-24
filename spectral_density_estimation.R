# estimate and graph spectral density using different windows

# estimate auto-variance
N = length(input_data$V1)
input_mean = mean(input_data$V1)
R = numeric(N-1)
R0 = 0
for(i in c(1:N))
{R0 = R0 + (input_data$V1[i] - input_mean)^2}
R0 = R0/N
for(k in c(1:(N-1)))
{
  R[k] = 0
  for(i in c(1:(N-k)))
  {R[k] = R[k] + (input_data$V1[i] - input_mean)*(input_data$V1[i+k] - input_mean)}
  R[k] = R[k]/N
}

# set M (10/20)
M = 20
# define lamda
lamda = numeric(N-1)
# truncated periodogram
for(i in c(1:(N-1)))
{
  if(i <= M)
  {lamda[i] = 1}
  else
  {lamda[i] = 0}
}
# triangular window
for(i in c(1:(N-1)))
{
  if(i <= M)
  {lamda[i] = 1 - i/M}
  else
  {lamda[i] = 0}
}
# Parzen window
for(i in c(1:(N-1)))
{
  if(i <= M){
  if(i/M <= 0.5)
    {lamda[i] = (1 - 6*((i/M)^2) + 6*((i/M)^3))}
  if(i/M >= 0.5 && i/M <= 1)
    {lamda[i] = 2*((1 - i/M)^3)}
  if(i/M > 1)
  {lamda[i] = 0}}
  else
  {lamda[i] = 0}
}
# estimate and graph the spectral density
spectrum = function(x) (R0 + 2*sum(lamda*cos(x*seq(1,N-1,by=1))*R))/(2*pi)
h = Vectorize(spectrum)
curve(h, xlim = c(0, pi), main = "Spectrum of 4.3.2 - Parzen Window", xlab = "Frequency", ylab = "Spectrum", col = "blue")

