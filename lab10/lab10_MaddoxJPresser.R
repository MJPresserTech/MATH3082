# Q1 Suppose that the initial distribution (of Xi’s) is Gamma with α = 5 and β = 2.
# (a) Find (theoretically) E (Xi) and V (Xi).

alpha <- 5
beta <- 2

E_Xi <- alpha / beta # E(Xi) = alpha / beta
V_Xi <- alpha / (beta^2) # V(Xi) = alpha / beta^2

cat("E(Xi) =", E_Xi, "\n")
cat("V(Xi) =", V_Xi, "\n")

# (b) Look at n = 4
# i) Create a sampling distribution for the sample mean using for loop.

set.seed(69420)

N <- 1000 # number of samples generated
n <- 4 # sample size
Xbars <- numeric(N)
for (i in 1:N){
    Xsamp <- rgamma(n, alpha, 1/beta)
    Xbars[i] <- mean(Xsamp)
}

# ii) Create a histogram and calculate the descriptive statistics for Xbars.
par(mfrow = c(1, 1))
hist(Xbars, breaks = 30, main = "Sampling Distribution of the Sample Mean", xlab = "Sample Mean", col = "lightblue", border = "black")

cat("Mean =", mean(Xbars), "\n")
cat("Variance =", var(Xbars), "\n")

# iii) Is the histogram approximately normal? Make a normal q-q plot.
par(mfrow = c(1, 1))
qqnorm(Xbars, main = "Q-Q Plot of Sample Means", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue")
qqline(Xbars, col = "red", lwd = 2)

# iv) What is (theoretically) E (X) and V (X)? How do these values compare to the descriptive statistics in part ii?
E_X <- E_Xi
V_X <- V_Xi / n

cat("Theoretical E(X) =", E_X, "\n")
cat("Theoretical V(X) =", V_X, "\n")

# (c) Repeat part (b) for n = 10. Does the histogram look more normal? If not increase n until it does.

n <- 10
Xbars <- numeric(N)
for (i in 1:N){
    Xsamp <- rgamma(n, alpha, 1/beta)
    Xbars[i] <- mean(Xsamp)
}

par(mfrow = c(1, 2))
hist(Xbars, breaks = 30, main = "Sampling Distribution of the Sample Mean (n=10)", xlab = "Sample Mean", col = "lightblue", border = "black")
cat("Mean =", mean(Xbars), "\n")
cat("Variance =", var(Xbars), "\n")

qqnorm(Xbars, main = "Q-Q Plot of Sample Means (n=10)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue")
qqline(Xbars, col = "red", lwd = 2)

E_X <- E_Xi
V_X <- V_Xi / n

cat("Theoretical E(X) =", E_X, "\n")
cat("Theoretical V(X) =", V_X, "\n")

n <- 12
Xbars <- numeric(N)
for (i in 1:N){
    Xsamp <- rgamma(n, alpha, 1/beta)
    Xbars[i] <- mean(Xsamp)
}

par(mfrow = c(1, 2))
hist(Xbars, breaks = 30, main = "Sampling Distribution of the Sample Mean (n=12)", xlab = "Sample Mean", col = "lightblue", border = "black")
cat("Mean =", mean(Xbars), "\n")
cat("Variance =", var(Xbars), "\n")

qqnorm(Xbars, main = "Q-Q Plot of Sample Means (n=12)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue")
qqline(Xbars, col = "red", lwd = 2)

E_X <- E_Xi
V_X <- V_Xi / n

cat("Theoretical E(X) =", E_X, "\n")
cat("Theoretical V(X) =", V_X, "\n")

# Q2
# (a) Try another probability distribution; say an exponential with mean 2. Repeat the process above and then increase the sample size until your distribution of sample means looks approximately normal.

n <- 4
mean_exp <- 2
rate_exp <- 1 / mean_exp

E_Xi <- mean_exp
V_Xi <- mean_exp^2

cat("E(Xi) =", E_Xi, "\n")
cat("V(Xi) =", V_Xi, "\n")

Xbars <- numeric(N)
for (i in 1:N){
    Xsamp <- rexp(n, rate = rate_exp)
    Xbars[i] <- mean(Xsamp)
}

par(mfrow = c(4, 2))
hist(Xbars, breaks = 30, main = "Exponential Sampling Distribution of the Sample Mean (n=4)", xlab = "Sample Mean", col = "lightblue", border = "black")
cat("Mean =", mean(Xbars), "\n")
cat("Variance =", var(Xbars), "\n")

qqnorm(Xbars, main = "Exponential Q-Q Plot of Sample Means (n=4)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue")
qqline(Xbars, col = "red", lwd = 2)

E_X <- E_Xi
V_X <- V_Xi / n

cat("Theoretical E(X) =", E_X, "\n")
cat("Theoretical V(X) =", V_X, "\n")

n <- 10
mean_exp <- 2
rate_exp <- 1 / mean_exp
Xbars <- numeric(N)
for (i in 1:N){
    Xsamp <- rexp(n, rate = rate_exp)
    Xbars[i] <- mean(Xsamp)
}

hist(Xbars, breaks = 30, main = "Exponential Sampling Distribution of the Sample Mean (n=10)", xlab = "Sample Mean", col = "lightblue", border = "black")
cat("Mean =", mean(Xbars), "\n")
cat("Variance =", var(Xbars), "\n")

qqnorm(Xbars, main = "Exponential Q-Q Plot of Sample Means (n=10)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue")
qqline(Xbars, col = "red", lwd = 2)

E_X <- E_Xi
V_X <- V_Xi / n

cat("Theoretical E(X) =", E_X, "\n")
cat("Theoretical V(X) =", V_X, "\n")

n <- 15
Xbars <- numeric(N)
for (i in 1:N){
    Xsamp <- rgamma(n, alpha, 1/beta)
    Xbars[i] <- mean(Xsamp)
}

hist(Xbars, breaks = 30, main = "Exponential Sampling Distribution of the Sample Mean (n=15)", xlab = "Sample Mean", col = "lightblue", border = "black")
cat("Mean =", mean(Xbars), "\n")
cat("Variance =", var(Xbars), "\n")

qqnorm(Xbars, main = "Exponential Q-Q Plot of Sample Means (n=15)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue")
qqline(Xbars, col = "red", lwd = 2)

E_X <- E_Xi
V_X <- V_Xi / n

cat("Theoretical E(X) =", E_X, "\n")
cat("Theoretical V(X) =", V_X, "\n")

n <- 30
Xbars <- numeric(N)
for (i in 1:N){
    Xsamp <- rgamma(n, alpha, 1/beta)
    Xbars[i] <- mean(Xsamp)
}

hist(Xbars, breaks = 30, main = "Exponential Sampling Distribution of the Sample Mean (n=30)", xlab = "Sample Mean", col = "lightblue", border = "black")
cat("Mean =", mean(Xbars), "\n")
cat("Variance =", var(Xbars), "\n")

qqnorm(Xbars, main = "Exponential Q-Q Plot of Sample Means (n=30)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue")
qqline(Xbars, col = "red", lwd = 2)

E_X <- E_Xi
V_X <- V_Xi / n

cat("Theoretical E(X) =", E_X, "\n")
cat("Theoretical V(X) =", V_X, "\n")

# Q3 Look at the sampling distribution of sample proportion ˆp = X/n. Note that X has the Binomial distribution. Sample proportion can be also thought of as a mean of a sample of Bernoulli (0 or 1) variables.
# (a) What is E (p) and V (p)?

# E(p) = E(X/n) = E(X)/n = np/n = p
# V(p) = V(X/n) = V(X)/n^2 = np(1-p)/n^2 = p(1-p)/n

# (b) How large a sample size do we need for the sampling distribution to be approximately normal? Try several values of p, say 0.1, 0.3, and 0.5. Relate your findings to the situation in Problems 1, 2.

p_values <- c(0.1, 0.3, 0.5)
n_values <- c(4, 10, 15, 30)
par(mfrow = c(6, 4), mar = c(2, 2, 2, 1))
for (n in n_values) {
    for (p in p_values) {
        Xbars <- numeric(N)
        for (i in 1:N){
            Xsamp <- rbinom(n, size = 1, prob = p)
            Xbars[i] <- mean(Xsamp)
        }
        hist(Xbars, breaks = 100, main = paste("Sampling Distribution of p-hat (p =", p, ", n =", n, ")"), xlab = "Sample Proportion", col = "lightblue", border = "black")
        cat("For p =", p, "and n =", n, "\n")
        cat("Mean =", mean(Xbars), "\n")
        cat("Variance =", var(Xbars), "\n")
        qqnorm(Xbars, main = paste("Q-Q Plot of p-hat (p =", p, ", n =", n, ")"), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue")
        qqline(Xbars, col = "red", lwd = 2)
        E_p <- p
        V_p <- p * (1 - p) / n
        cat("Theoretical E(p) =", E_p, "\n")
        cat("Theoretical V(p) =", V_p, "\n")
        cat("--------------------------------------", "\n")
    }
}
