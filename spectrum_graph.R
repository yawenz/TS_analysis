# spectrum graph
# 4.1.2
spectrum = function(x) 1/(2*pi*(1.49-1.4*cos(x)))
curve(spectrum, xlim = c(0, pi), main = "Spectrum of 4.1.2", xlab = "Frequency", ylab = "Spectrum", col = "blue")

# 4.1.3
spectrum = function(x) (1.81+1.8*cos(x))/(2*pi*(1.81-1.8*cos(x)))
curve(spectrum, xlim = c(0, pi), main = "Spectrum of 4.1.3", xlab = "Frequency", ylab = "Spectrum", col = "blue")

# 4.1.7
spectrum = function(x) 1/(2*pi*(4.4436-6.208*cos(x)+1.88*cos(2*x)))
curve(spectrum, xlim = c(0, pi), main = "Spectrum of 4.1.7", xlab = "Frequency", ylab = "Spectrum", col = "blue")

# get local maximum
# optimize(spectrum, interval = c(0, pi), maximum = TRUE)

# 4.1.8
spectrum = function(x) (5.5125-7.41*cos(x)+1.9*cos(2*x))/(2*pi*(4.53-6.12*cos(x)+1.6*cos(2*x)))
curve(spectrum, xlim = c(0, pi), main = "Spectrum of 4.1.8", xlab = "Frequency", ylab = "Spectrum", col = "blue")

# 4.1.10
spectrum = function(x) 1/(2*pi*(5.5125-7.41*cos(x)+1.9*cos(2*x))*(4.7+6.46*cos(x)+1.8*cos(2*x)))
curve(spectrum, xlim = c(0, pi), main = "Spectrum of 4.1.10", xlab = "Frequency", ylab = "Spectrum", col = "blue")

# 4.2.1
periodogram = function(x) (1/(2*pi*30))*(sum(input_data$V1*cos(x*seq(1,30,by=1)))^2+sum(input_data$V1*sin(x*seq(1,30,by=1)))^2)
h <- Vectorize(periodogram)
curve(h, 0, pi, n = 16, main = "Periodogram of 4.2.1", xlab = "Frequency", ylab = "Periodogram", col = "red")
