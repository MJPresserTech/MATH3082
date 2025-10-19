# Q1 a) Make calculations with a variety of distributions given below. See how closely the 68-95-99.7 guideline is met. Put the results into the Table.

set.seed(12345)

# Normal
mu_norm <- 3 # Mean
var_norm <- 9 # Variance
sd_norm <- sqrt(var_norm) # Standard deviation

# Compute probabilities within ±1σ, ±2σ, ±3σ
norm_1_std <- (pnorm(mu_norm + 1*sd_norm, mu_norm, sd_norm) - pnorm(mu_norm - 1*sd_norm, mu_norm, sd_norm)) * 100
norm_2_std <- (pnorm(mu_norm + 2*sd_norm, mu_norm, sd_norm) - pnorm(mu_norm - 2*sd_norm, mu_norm, sd_norm)) * 100
norm_3_std <- (pnorm(mu_norm + 3*sd_norm, mu_norm, sd_norm) - pnorm(mu_norm - 3*sd_norm, mu_norm, sd_norm)) * 100

# Exponential
mu_exp <- 5
rate_exp <- 1 / mu_exp
sd_exp <- 1 / rate_exp

exp_1_std <- (pexp(mu_exp + 1*sd_exp, rate_exp) - pexp(max(0, mu_exp - 1*sd_exp), rate_exp)) * 100
exp_2_std <- (pexp(mu_exp + 2*sd_exp, rate_exp) - pexp(max(0, mu_exp - 2*sd_exp), rate_exp)) * 100
exp_3_std <- (pexp(mu_exp + 3*sd_exp, rate_exp) - pexp(max(0, mu_exp - 3*sd_exp), rate_exp)) * 100

# Uniform
lower_uni <- 1 # Lower bound (a)
upper_uni <- 2 # Upper bound (b)

mu_uni <- (lower_uni + upper_uni) / 2 # Mean = (a + b)/2
sd_uni <- (upper_uni - lower_uni) / sqrt(12) # SD = (b - a)/√12
uni_1_std <- (punif(mu_uni + 1*sd_uni, lower_uni, upper_uni) - punif(mu_uni - 1*sd_uni, lower_uni, upper_uni)) * 100
uni_2_std <- (punif(mu_uni + 2*sd_uni, lower_uni, upper_uni) - punif(mu_uni - 2*sd_uni, lower_uni, upper_uni)) * 100
uni_3_std <- (punif(mu_uni + 3*sd_uni, lower_uni, upper_uni) - punif(mu_uni - 3*sd_uni, lower_uni, upper_uni)) * 100


table <- data.frame(
  Distribution = c("Normal (3,9)", "Exponential (mean=5)", "Uniform (1,2)"),
  Within_1_SD = c(norm_1_std, exp_1_std, uni_1_std),
  Within_2_SD = c(norm_2_std, exp_2_std, uni_2_std),
  Within_3_SD = c(norm_3_std, exp_3_std, uni_3_std)
)

print(table)

# Q2 a) Q-Q plots

data <- read.csv("lab7/Dist-exam.txt")

data$C1 <- as.numeric(data$C1) # Make column numeric
par(mfrow = c(1, 2))

hist(data$C1,
     main = "Histogram of Exam Scores",
     xlab = "Score",
     col = "lightblue",
     border = "black")

qqnorm(data$C1, main = "Q-Q Plot: Exam Scores vs Normal Distribution")
qqline(data$C1, col = "red")

# Q3 a) 

n <- 1000 # sample size

x_uniform <- runif(n, min = -1, max = 1) # Uniform(-1,1)
x_t3 <- rt(n, df = 3) # t-distribution with 3 degrees of freedom

par(mfrow = c(1, 2))

qqnorm(x_uniform, main = "Normal Q-Q: Uniform(-1,1) (light tails)")
qqline(x_uniform, col = "red")

qqnorm(x_t3, main = "Normal Q-Q: t(3) (heavy tails)")
qqline(x_t3, col = "red")

# Q3 b)

data <- read.csv("lab7/DowJones.txt")

par(mfrow = c(1, 2))  
hist(data$x,
     main = "Histogram of Dow Jones Returns",
     xlab = "Return (%)",
     col = "lightblue",
     border = "black")

qqnorm(data$x, main = "Normal Q-Q Plot of Dow Jones Returns")
qqline(data$x, col = "red")
