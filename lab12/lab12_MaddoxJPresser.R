# install.packages("TeachingDemos")
library(TeachingDemos)

# exists("z.test") # finally

# (a) In the file sample1.csv, based on the sample in first column, compute
# 95% and 99% C.I.’s for the mean (assume σ = 1.) Since columns have
# different length, the shorter one is padded with NA (“not available”)
# values. Before doing the analysis, we need to remove them.

dat = read.csv("lab12/sample1.csv")
x1 = dat[,1]
x1 = x1[!is.na(x1)]
cat("ztest at conf.level = 0.95 \n")
z.test(x1, sd = 1, alternative = "two.sided", conf.level = 0.95)

# The function z.test will simultaneously do the test and produce a C.I.
# Which one is wider, 95% or 99% C.I., and why?
cat("ztest at conf.level = 0.99 \n")
z.test(x1, sd = 1, alternative = "two.sided", conf.level = 0.99)

# (b) Based on the sample in the second column, compute 95% C.I. for the
# mean. Compare with the part (a) 95% C.I. Which one is wider and why?

x2 = dat[,2]
x2 = x2[!is.na(x2)]
cat("ztest at conf.level = 0.95 \n")
z.test(x2, sd = 1, alternative = "two.sided", conf.level = 0.95)

# (c) Can you compute a 100% C.I.? Why or why not?

# No, because the confidence level of 100% would require a z-score of infinity.

# (d) Do a simulation study generating 100 samples from the standard Normal
# distribution, then calculate 90% C.I. for each sample. How many of 90%
# C.I.’s out of a 100 would you expect to cover your true mean? Count
# how many among your results contained the true mean 0.
# [Hint: You need to write a for loop. It might be easier to do the calculations
# on your own, not relying on z.test function. If you want to use z.test, you
# can try the following: z1 = z.test(x1, sd = 1, conf.level = 0.90); CI
# = z1[[4]] ]

set.seed(123)
n = 100
count = 0
for (i in 1:n) {
  sample = rnorm(30) # generate a sample of size 30
  z1 = z.test(sample, sd = 1, conf.level = 0.90)
  CI = z1[[4]]
  if (CI[1] <= 0 && CI[2] >= 0) {
    count = count + 1
  }
}
cat("Number of 90% C.I.'s that contain the true mean 0:", count, "\n")
cat("Expected number of 90% C.I.'s that contain the true mean 0:", n * 0.90, "\n")