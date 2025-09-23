    # lab 4

    # Q1)
    nBern = 50*6
    set.seed(3129)
    x = sample(c(0,1), nBern, replace = T, prob=c(0.7, 0.3))
    x = matrix(x, 50, 6) # matrix with 50 rows and 6 columns
    x

    # compute # of successes in each row
    y = apply(x, 1, sum) ## ..., 2, ... will be column sums
    y

    hist(y, breaks=seq(-0.5,6.5,1), main="Histogram of # of successes in 50 trials", xlab="# of successes", col="red")

    # Q1b)
    print(sum(y))

    # Q1c)

    # Q1d) 
    py = dbinom(0:6,6,0.3)
    y = rbinom(500, size = 6, prob = 0.3)
    par(mfrow=c(1,2))
    barplot(py, names.arg = 0:6, main="dbinom", xlab="y", col="lightblue")
    hist(y, breaks=seq(-0.5,6.5,1), main="rbinom", xlab="y", col="red")
    
    # Q2a)