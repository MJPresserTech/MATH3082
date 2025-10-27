# A discrete distribution that we have used for integer durations (waiting times) is Geometric. 
# It can be obtained from rounding the Exponential values.

# a) Generate 500 Exponential r.v.’s with a mean 88. Now, round the values up (use ceiling function).
set.seed(69420)
mu <- 88
exp_values <- rexp(500, rate = 1/mu)
c_exp_values <- ceiling(exp_values)

# b) Make a graph and convince yourselves that you are dealing with Geometric distribution.
par(mfrow = c(1, 1))
hist(c_exp_values, 
     breaks = seq(-0.5, max(c_exp_values) + 0.5, by = 1), # fixes breaks to integers to make it look better
     main = "Histogram of Rounded Exponential Values", 
     xlab = "Rounded Values", 
     ylab = "Frequency", 
     col = "lightblue",
     border = "blue") # all that can be seen are the borders

# c) What is the parameter p of this distribution? [Hint: try to evaluate P(Y=1) using Exponential CDF.
p <- pexp(1, rate = 1/mu) # P(Y=1) = P(0 < X ≤ 1) = F(1) - F(0) = F(1)
print(p)

# d) Verify your guess by generating a sample from Geometric (rgeom function) with that parameter and comparing results.
geom_sample <- rgeom(500, prob = p) + 1  # +1 because rgeom gives number aof failures before first success

par(mfrow = c(1, 2))

hist(c_exp_values, 
     breaks = seq(-0.5, max(c_exp_values) + 0.5, by = 1),
     main = "Histogram of Rounded Exponential Values", 
     xlab = "Rounded Values", 
     ylab = "Frequency", 
     col = "lightblue")

hist(geom_sample, 
     breaks = seq(-0.5, max(geom_sample) + 0.5, by = 1), 
     main = "Histogram of Geometric Distribution Sample", 
     xlab = "Values", 
     ylab = "Frequency", 
     col = "purple")


# 2) Generate a sample of 200 Gamma(α = 3, β = 0.1) r.v.’s by the summation method

alpha <- 3
beta <- 0.1
rate <- 1 / beta
n <- 200

gamma_sample <- numeric(n) # init

for (i in 1:n) {
  exp_values <- rexp(alpha, rate = rate)  # generate 3 exponentials
  gamma_sample[i] <- sum(exp_values) # gamma = sum of exponentials at alpha = 1
}

par(mfrow = c(1, 1))

hist(gamma_sample,
     breaks = 20,
     col = "orange",
     main = "Histogram of Gamma(α=3, β=0.1) Sample",
     xlab = "Values",
     ylab = "Frequency")

# 3) Poisson stuff
# a) Make a dot plot of first 25 event times (Hint: make a new vector with only the first 25 values. Use the value of "y" equal to a vector of ones)
data <- read.csv("lab8/eq_time.csv")
event_times <- data$time[1:25]
y_values <- rep(1, 25) # y values all set to 1

par(mfrow = c(1, 1))
plot(event_times, y_values, 
     xlab = "Event Times", 
     ylab = "", 
     main = "Dot Plot of First 25 Event Times", 
     yaxt = 'n', # no y axis
     pch = 19,
     col = "blue")

# b) Compute the differences between event times to obtain interarrival times for earthquakes. Make a histogram of the results. Do they appear exponentially distributed?
interarrival_times <- diff(event_times) # differences between even time values

par(mfrow = c(1, 1))
hist(interarrival_times,
     breaks = 20,
     col = "lightgreen",
     main = "Histogram of Interarrival Times",
     xlab = "Interarrival Times",
     ylab = "Frequency")

# c) Estimate the mean time between earthquakes in minutes.
mean_interarrival_days <- mean(interarrival_times)
mean_interarrival_minutes <- mean_interarrival_days * 24 * 60

print(mean_interarrival_minutes)

# d) Make a histogram of the even times.
par(mfrow = c(1, 1))
hist(event_times,
     breaks = 20,
     col = "lightcoral",
     main = "Histogram of Event Times",
     xlab = "Event Times",
     ylab = "Frequency")