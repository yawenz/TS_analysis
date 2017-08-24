# plot the filters graph

# 5.1.2
filter = function(x) (2+2*cos(x))/(1.25-cos(x))
curve(filter, xlim = c(0, pi), main = "Transfer Function of 5.1.2", xlab = "Frequency", ylab = "", col = "blue")

# 5.1.4
filter = function(x) 0.25*(1-cos(x))*(1-cos(x))
curve(filter, xlim = c(0, pi), main = "Transfer Function of 5.1.4", xlab = "Frequency", ylab = "", col = "blue")

# 5.1.7
filter = function(x) 0.0004*(6+8*cos(x)+2*cos(2*x))/(3.847602-5.033202*cos(x)+1.282*cos(2*x))
curve(filter, xlim = c(0, pi), main = "Transfer Function of 5.1.7", xlab = "Frequency", ylab = "", col = "blue")

# 5.1.11
filter = function(x) 0.009801*(1-cos(x))*(1-cos(x))/(2.866884+3.78368*cos(x)+0.956*cos(2*x))
curve(filter, xlim = c(0, pi), main = "Transfer Function of 5.1.11", xlab = "Frequency", ylab = "", col = "blue")

# 5.1.15
filter = function(x) 0.570025*(2.648025-3.22*cos(x)+2*cos(2*x))/(1.628549-1.83314*cos(x)+1.02*cos(2*x))
curve(filter, xlim = c(0, pi), main = "Transfer Function of 5.1.15", xlab = "Frequency", ylab = "", col = "blue")

# 5.1.16
filter = function(x) 0.06125*(1-cos(2*x))/(2.525802+3.466198*cos(x)+1.298*cos(2*x))
curve(filter, xlim = c(0, pi), main = "Transfer Function of 5.1.16", xlab = "Frequency", ylab = "", col = "blue")
