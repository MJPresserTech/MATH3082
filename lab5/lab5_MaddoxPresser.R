# lab 5

# Q1 (a) Compute the average number of Hurricanes per year (we will use it as an estimate of μ). 

data <- read.csv("lab5/hurr1.csv")
avg <- mean(data$hurr)
print(avg)

# Q1 (b) Obtain the histogram of the data and overlay it with a plot of exact Poisson probabilities. 
# Q1 (b_a) Make sure the histogram is in the density format. To do this, use hist(x, freq = F). 
# Q1 (b_b) To overlay the plot with probabilities, use lines(0:15, dpois(0:15, avg)) where avg is the mean number of hurricanes per year. 
par(mfrow = c(1, 1))
hist(as.numeric(data$hurr), freq = F, breaks = seq(-0.5, max(data$hurr)+0.5, by=1), main="Histogram of Hurricanes per Year with Poisson Overlay", xlab="Number of Hurricanes", ylab="Density", col="lightblue")
lines(0:15, dpois(0:15, avg), col = "blue", lwd = 2)

# Q1 (c) Does the theoretical distribution (Poisson) appear to fit the data? (To see what kind of fit can be considered satisfactory, you might simulate 150 Poisson values and make their histograms a couple of times.)

set.seed(69420)
simulated_data_1 <- rpois(150, avg)
set.seed(9292025)
simulated_data_2 <- rpois(150, avg)
set.seed(1234567890)
simulated_data_3 <- rpois(150, avg)
set.seed(987654321)
simulated_data_4 <- rpois(150, avg)

par(mfrow = c(2, 2))
hist(simulated_data_1, freq = F, breaks = seq(-0.5, max(simulated_data_1)+0.5, by=1), main="Histogram of Simulated Poisson Data", xlab="Number of Events", ylab="Density", col="lightgreen", sub = "Seed = 69420")
lines(0:15, dpois(0:15, avg), col = "red", lwd = 2)
hist(simulated_data_2, freq = F, breaks = seq(-0.5, max(simulated_data_2)+0.5, by=1), main="Histogram of Simulated Poisson Data", xlab="Number of Events", ylab="Density", col="lightgreen", sub = "Seed = 9292025")
lines(0:15, dpois(0:15, avg), col = "red", lwd = 2)
hist(simulated_data_3, freq = F, breaks = seq(-0.5, max(simulated_data_3)+0.5, by=1), main="Histogram of Simulated Poisson Data", xlab="Number of Events", ylab="Density", col="lightgreen", sub = "Seed = 1234567890")
lines(0:15, dpois(0:15, avg), col = "red", lwd = 2)
hist(simulated_data_4, freq = F, breaks = seq(-0.5, max(simulated_data_4)+0.5, by=1), main="Histogram of Simulated Poisson Data", xlab="Number of Events", ylab="Density", col="lightgreen", sub = "Seed = 987654321")
lines(0:15, dpois(0:15, avg), col = "red", lwd = 2)



# Q1 (d) In 2005, 15 hurricanes occurred. Is this highly unusual? Compute the probability that at any given year, there will be at least (≥) 15 hurricanes. As a result of this Problem, you may conclude that 2005 number contradicts the decades-long trend. This may mean that either Poisson model does not completely work or the climate is changing

tot_data = length(data$hurr)
at_least_15 = sum(data$hurr >= 15) # count how many years had at least 15 hurricanes
prob_at_least_15 = at_least_15 / tot_data
theo_prob_at_least_15 = 1 - ppois(14, avg) # P(X >= 15) = 1 - P(X <= 14)
print(prob_at_least_15)
print(theo_prob_at_least_15)




# Q2 Now, let’s consider the number of hurricanes per decade. 
# Q2 (a) Obtain a plot of the Poisson distribution representing the number of hurricanes per decade (what value of μ are you going to use?).
decade_avg <- avg * 10

par(mfrow = c(1, 1))
barplot(dpois(0:150, decade_avg), names.arg=0:150, main="Poisson Distribution of Hurricanes per Decade", xlab="Number of Hurricanes", ylab="Probability", col="lightcoral")

# Q2 (b) In the 2001-2010 decade, 78 hurricanes occurred. Is this highly unusual?
theo_prob_at_least_78 <- 1 - ppois(77, decade_avg)
print(theo_prob_at_least_78)



# Q4 (a) Calculate a vector of Poisson probabilities with the mean 5, for x-values from 0 to 10.
mean = 5
x_values <- 0:10
poisson_probs <- dpois(x_values, mean)
print(round(poisson_probs, 4)) # round function because numbers really long

# Q4 (b) Calculate vectors of Binomial probabilities for the following values: n = 20, p = 0.25, n = 100, p = ? (choose the value of p to match the mean of 5) n = 500, p = ?
# Compare the Binomial results to Poisson. [Hint: If you’re going to print the numerical values, use round function to avoid excessive precision. For example, round(pi, 4)]
n1 <- 20
p1 <- 0.25
binom_probs1 <- dbinom(x_values, n1, p1)
print(round(binom_probs1, 4))

n2 <- 100
mean2 <- 5
p2 <- mean2 / n2
binom_probs2 <- dbinom(x_values, n2, p2)
print(round(binom_probs2, 4))

n3 <- 500
mean3 <- 5
p3 <- mean3 / n3
binom_probs3 <- dbinom(x_values, n3, p3)
print(round(binom_probs3, 4))

ratio_1 <- binom_probs1 / poisson_probs
ratio_2 <- binom_probs2 / poisson_probs
ratio_3 <- binom_probs3 / poisson_probs

print(mean(ratio_1))
print(mean(ratio_2))
print(mean(ratio_3))