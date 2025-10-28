# a)
set.seed(69420)
uni1 = runif(50, 0, 1)
uni2 = runif(50, 0, 1)
uni3 = runif(50, 0, 1)

par(mfrow=c(1,3))
hist(uni1, main="Histogram of uni1", xlab="Value", col="lightblue", border="black")
hist(uni2, main="Histogram of uni2", xlab="Value", col="lightgreen", border="black")
hist(uni3, main="Histogram of uni3", xlab="Value", col="lightcoral", border="black")

mu1 = mean(uni1)
mu2 = mean(uni2)
mu3 = mean(uni3)

cat("Mean of uni1:", mu1, "\n")
cat("Mean of uni2:", mu2, "\n")
cat("Mean of uni3:", mu3, "\n")

theoretical_mean = 1 - 0 / 2 # Theoretical mean of U(a, b) = (a + b) / 2
cat("Theoretical mean of U(0, 1):", theoretical_mean, "\n")

# b) 
s = runif(50000, 0, 1)
par(mfrow=c(1,1))
hist(s, main="Histogram of s", xlab="Value", col="lightblue", border="black")
s_mean = mean(s)
cat("Mean of s:", s_mean, "\n")

socorro = read.csv("lab9/SocorroPrec.csv")
par(mfrow=c(1,1))
hist(socorro$x, main="Histogram of Socorro Precipitation", xlab="Precipitation", col="lightgreen", border="black")
