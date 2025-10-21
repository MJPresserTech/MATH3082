# A discrete distribution that we have used for integer durations (waiting times) is Geometric. 
# It can be obtained from rounding the Exponential values.

# a) Generate 500 Exponential r.v.’s with a mean 88. Now, round the values up (use ceiling function).
set.seed(69420)
mu <- 88
exp_values <- rexp(500, rate = 1/mu)
c_exp_values <- ceiling(exp_values)

# b) Make a graph and convince yourselves that you are dealing with Geometric distribution.
par(mfrow = c(1, 1))
hist(c_exp_values, breaks = seq(-0.5, max(c_exp_values) + 0.5, by = 1), 
     main = "Histogram of Rounded Exponential Values", 
     xlab = "Rounded Values", 
     ylab = "Frequency", 
     col = "lightblue")

# c) What is the parameter p of this distribution? [Hint: try to evaluate P(Y=1) using Exponential CDF.
p <- pexp(1, rate = 1/mu) # P(Y=1) = P(0 < X ≤ 1) = F(1) - F(0) = F(1)
print(p)

# d) Verify your guess by generating a sample from Geometric (rgeom function) with that parameter and comparing results.
geom_sample <- rgeom(500, prob = p) + 1  # +1 because rgeom gives number of failures before first success

par(mfrow = c(1, 2))

hist(c_exp_values, breaks = seq(-0.5, max(c_exp_values) + 0.5, by = 1), 
     main = "Histogram of Rounded Exponential Values", 
     xlab = "Rounded Values", 
     ylab = "Frequency", 
     col = "lightblue")

hist(geom_sample, breaks = seq(-0.5, max(geom_sample) + 0.5, by = 1), 
     main = "Histogram of Geometric Distribution Sample", 
     xlab = "Values", 
     ylab = "Frequency", 
     col = "purple")


# Q2) 