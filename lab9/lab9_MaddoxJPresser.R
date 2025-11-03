# q1 a)
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

theoretical_mean = (1 - 0) / 2 # Theoretical mean of U(a, b) = (a + b) / 2
cat("Theoretical mean of U(0, 1):", theoretical_mean, "\n")

# b) 
s = runif(50000, 0, 1)
par(mfrow=c(1,1))
hist(s, main="Histogram of s", xlab="Value", col="lightblue", border="black")
s_mean = mean(s)
cat("Mean of s:", s_mean, "\n")

# q2 a)

socorro = read.csv("lab9/SocorroPrec.csv")
x = socorro[,1]
bhat = mean(x)
par(mfrow=c(1,1))
hist(x, breaks=seq(0,3.5,by=0.1), freq=F)
xc = seq(0,3.5,by=0.02)
yc = dexp(xc, 1/bhat)
# recall that 1/beta is the default parameter in R
lines(xc, yc, lwd=2, col="red")

# b)
xbar = mean(x)
s2 = var(x)

beta_hat = s2 / xbar
alpha_hat = xbar^2 / s2

lines(xc, dgamma(xc, shape=alpha_hat, scale=beta_hat),
      lwd=2, col="blue")
legend("topright",
       legend = c("Exponential", "Gamma"),
       col = c("red", "blue"),
       lwd = 2)

cat("alpha_hat =", alpha_hat, " beta_hat =", beta_hat, "\n")

# q3 a)
par(mfrow=c(1,2))

newcomb <- read.csv("lab9/newcomb.txt")
x <- newcomb$x

mean_with_outliers <- mean(newcomb$x)
median_with_outliers <- median(newcomb$x)
sd_with_outliers <- sd(newcomb$x)
iqr_with_outliers <- IQR(newcomb$x)
boxplot(newcomb$x, 
      main = "Boxplot of Newcomb data (with outliers)", 
      ylab = "x", 
      col = "lightblue", 
      border = "black")

newcomb_without_outliers <- subset(newcomb, x > 0)

mean_without_outliers <- mean(newcomb_without_outliers$x)
median_without_outliers <- median(newcomb_without_outliers$x)
sd_without_outliers <- sd(newcomb_without_outliers$x)
iqr_without_outliers <- IQR(newcomb_without_outliers$x)
boxplot(newcomb_without_outliers$x,
      main = "Boxplot of Newcomb data (without outliers)", 
      ylab = "x", 
      col = "lightgreen", 
      border = "black")

results <- data.frame(
  Statistic = c("Mean", "Median", "St.dev.", "IQR"),
  With_outliers = c(mean_with_outliers, median_with_outliers, sd_with_outliers, iqr_with_outliers),
  Without_outliers = c(mean_without_outliers, median_without_outliers, sd_without_outliers, iqr_without_outliers)
)
print(results)

# q4 a)
par(mfrow=c(1,1))

beta <- 1
n <- 10000
meds <- numeric(n)

for (i in 1:n) {
  x <- rexp(5, rate = 1/beta)
  meds[i] <- median(x)
}

# q4 b)
hist(meds,
     breaks = 40,
     col = "lightblue",
     main = "Sampling Distribution of the Sample Median (n = 5)",
     xlab = "Sample Median")

mean_meds <- mean(meds)
sd_meds <- sd(meds)

cat("Mean of sample medians:", mean_meds, "\n")
cat("Standard deviation of sample medians:", sd_meds, "\n")

# q4 c) 

mu <- beta * log(2)
cat("Theoretical mean of the sample median:", mu, "\n")

# q4 d)

beta_hat1 <- meds[1] / mu
cat("Beta hat from first sample median:", beta_hat1, "\n")

sd_beta_hat <- sd_meds / mu
cat("Standard deviation of beta hat:", sd_beta_hat, "\n")