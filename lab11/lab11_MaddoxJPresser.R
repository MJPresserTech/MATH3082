# Problem 1

# (a) To get a feel for correlation: make scatterplots of Y vs X and compute
# the sample correlation cor(x,y) for 4 examples from the file corex.csv.
# Notice how the shape and orientation of plots is changing according to
# the value of r. Describe what you see.

data_corex <- read.csv("lab11/corex.csv")
par(mfrow=c(2,2))
for (i in 1:4) {
  x <- data_corex[,2*i-1] # X in 1, 3, 5, 7
  y <- data_corex[,2*i]  # Y in 2, 4, 6, 8
  plot(x, y, main=paste("Scatterplot of Y_", i, " vs X_", i, " (r =", cor(x,y), ")"),
       xlab="X", ylab="Y")
  abline(lm(y ~ x), col="red")
  cat("Sample correlation (r) for plot", i, ":", cor(x, y), "\n")
}

# (b) What is the correlation of X with itself?
for (i in 1:4) {
  x <- data[,2*i-1]
  cat("Correlation of X_", i, " with itself:", cor(x, x), "\n")
}

# (c) Compute the correlation for temperature data between two cities: Tuc-
# son, AZ and Eugene, OR. (file 2cities.csv). The temperatures are
# given in Celcius. Does the correlation change if we apply a linear func-
# tion to X or Y, say, we convert the temperature to Fahrenheit (recall
# that oF = oC × 1.8 + 32)?

data_2cities <- read.csv("lab11/2cities.csv")
tucson_temp_C <- data_2cities$tucson
eugene_temp_C <- data_2cities$eugene
cor_C <- cor(tucson_temp_C, eugene_temp_C)
cat("Correlation in Celsius:", cor_C, "\n")
tucson_temp_F <- tucson_temp_C * 1.8 + 32
eugene_temp_F <- eugene_temp_C * 1.8 + 32
cor_F <- cor(tucson_temp_F, eugene_temp_F)
cat("Correlation in Fahrenheit:", cor_F, "\n")

# (d) If X and Y are independent then their correlation is 0. The converse
# is not true.
# As an example, consider the data on fuel economy for Ford Escort
# (Escort.csv) The variable X is speed in km/h, and Y is the fuel con-
# sumption in liters per 100km. Make a scatterplot. The correlation be-
# tween the speed and fuel consumption is close to 0, however, there is a
# clear relationship between the two. In fact, the correlation reflects the
# strength of linear relationship.

par(mfrow=c(1,1))
data_escort <- read.csv("lab11/Escort.csv")
speed <- data_escort$Speed
fuel_consumption <- data_escort$Fuel
plot(speed, fuel_consumption, main="Scatterplot of Speed vs Fuel Consumption",
     xlab="Speed (km/h)", ylab="Fuel Consumption (liters per 100km)")
abline(lm(fuel_consumption ~ speed), col="red")
cat("Correlation between speed and fuel consumption:", cor(speed, fuel_consumption), "\n")

# Problem 2

# (a) Consider the data set in MercBass.csv. The data contain several environmental
# variables for a sample of Florida lakes, and the average mercury content in
# bass caught there.
# Obtain the “matrix” scatterplot (pairs(dat)) for all the continuous variables.
# Also, compute all the pairwise correlations (cor(dat)).
# Which variables seem to be related? Do the correlation values fully reflect the
# extent of relationship between the variables?

data_mercbass <- read.csv("lab11/MercBass.csv")
numeric_vars <- data_mercbass[, sapply(data_mercbass, is.numeric)] # choose only numeric columns

pairs(numeric_vars, main="Matrix Scatterplot of MercBass Data")
cor_matrix <- cor(numeric_vars)
print(cor_matrix)
# display correlations greater than 0.7 or less than -0.7
high_correlations <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) != 1, arr.ind=TRUE) # which() to find indices, arr.ind=TRUE to get row/col indices
for (i in 1:nrow(high_correlations)) { # iterate through found indices in high_correlations
  row <- high_correlations[i, 1]
  col <- high_correlations[i, 2]
  cat("High correlation between", colnames(cor_matrix)[row], "and", colnames(cor_matrix)[col], ":", cor_matrix[row, col], "\n")
}

# Problem 4
# Generate two vectors of length 1000 of Uniform [0,1] RV’s. Based on these,
# obtain:

# (a) a sample from Exponential(1) distribution. Make a histogram to make
# sure it “looks right”. How would you generate Exponential(64) distribu-
# tion?

set.seed(42069)
par(mfrow=c(2,1))
u = runif(1000)
lambda_1 = 1
exp_sample <- -log(1-u) / lambda_1 # U = 1 - exp(- lambda X)  =>  1-U = exp(- lambda X)  =>  -ln(1-U)/lambda = X
hist(exp_sample, main="Histogram of Exponential(1) Sample", xlab="Value", breaks=30, col="lightblue", border="black")
lambda_64 = 64
exp_64_sample <- -log(1-u) / lambda_64
hist(exp_64_sample, main="Histogram of Exponential(64) Sample", xlab="Value", breaks=30, col="lightgreen", border="black")

# (b) two samples (columns) of standard Normal distribution. Make his-
# tograms and a scatterplot to check independence.2
# check if from here on is correct Maddox
par(mfrow=c(2,2))
norm_samples <- matrix(rnorm(2000), ncol=2)
hist(norm_samples[,1], main="Histogram of First Normal Sample", xlab="Value")
hist(norm_samples[,2], main="Histogram of Second Normal Sample", xlab="Value")
plot(norm_samples[,1], norm_samples[,2], main="Scatterplot of Two Normal Samples",
     xlab="First Normal Sample", ylab="Second Normal Sample")

# (c) Make a scatterplot of the two original Uniform [0,1] RV’s, compare its
# shape with the plot of Normals from part (b).

plot(runif(1000), runif(1000), main="Scatterplot of Two Uniform Samples",
     xlab="First Uniform Sample", ylab="Second Uniform Sample")

# (d) Using Uniform [0,1] RV’s, obtain a sample from a discrete distribution
# given by the table: x 0 1 2
# p(x) 0.1 0.3 0.6
# [Hint: think how the inverse CDF for X will look like.]
# You know that correlation = 0 does not imply independence. But practically, it’s a
# fairly good test, as long as you look at the scatterplot to pick out non-linear dependencies.

u <- runif(1000)
discrete_sample <- cut(u, breaks=c(-Inf, 0.1, 0.4, Inf), labels=c(0, 1, 2))
table(discrete_sample)