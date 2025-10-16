# Q1 a) Generate samples from the Exponential (mean = 7) distribution in two ways: i) Directly from Exponential (7), ii) first obtain a sample from Exp(1) random variable, then multiply it by 7.
set.seed(69420)
n <- 1000

samples_direct <- rexp(n, rate = 1/7)
samples_scaled <- rexp(n, rate = 1) * 7

par(mfrow=c(1,2))
hist(samples_direct, main="Direct Exponential(7)", xlab="Value", col="lightblue", border="black")
hist(samples_scaled, main="Scaled Exponential(1)", xlab="Value", col="lightgreen", border="black")

# Q1 b) (b) Compute the mean of the resulting sample and compare it to the theoretical mean. Do the same for variance.
mean_direct <- mean(samples_direct)
var_direct <- var(samples_direct)
mean_scaled <- mean(samples_scaled)
var_scaled <- var(samples_scaled)
theoretical_mean <- 7
theoretical_var <- theoretical_mean^2

# table
results <- data.frame(
  Method = c("Direct Exponential(7)", "Scaled Exponential(1)"),
  Sample_Mean = c(mean_direct, mean_scaled),
  Sample_Var = c(var_direct, var_scaled),
  Theoretical_Mean = theoretical_mean,
  Theoretical_Var = theoretical_var
)
print(results)

# Q3 a) Generate a random sample of size 1000 from a geometric distribution with a probability of success p = 0.3.
set.seed(69420)
n <- 1000
p <- 0.3
geo_samples <- rgeom(n, prob = p)
print(geo_samples)

# Q3 b) Calculate the mean and variance of the generated sample.
mean_geo <- mean(geo_samples)
var_geo <- var(geo_samples)
print(mean_geo)
print(var_geo)

# Q3 c) Plot the probability mass function (PMF) of the geometric distribution. Use x = 1,2, ... ,10.
x <- 1:10
pmf <- dgeom(x - 1, prob = p)
par(mfrow=c(1,1))
barplot(pmf, names.arg = x, main = "PMF of Geometric Distribution", xlab = "x", ylab = "Probability", col = "lightblue")

# Q3 d) (d) Calculate and plot the cumulative distribution function (CDF) of the geometric distribution.
cdf <- pgeom(x - 1, prob = p)
plot(x, cdf, type = "s", main = "CDF of Geometric Distribution", xlab = "x", ylab = "Cumulative Probability", col = "blue", lwd = 2)
