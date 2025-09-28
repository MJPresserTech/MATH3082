    # lab 4

    # Q1)
    nBern = 50*6
    set.seed(3129)
    x = sample(c(0,1), nBern, replace = T, prob=c(0.7, 0.3))
    x = matrix(x, 50, 6) # matrix with 50 rows and 6 columns

    # compute # of successes in each row
    y = apply(x, 1, sum) ## ..., 2, ... will be column sums

    par(mfrow=c(1,1))
    hist(y, breaks=seq(-0.5,6.5,1), main="Histogram of # of successes in 50 trials (Y)", xlab="# of successes", col="red")

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
    py = dhyper(0:5,4,6,5) # k=4 is the 2nd number and N-k=6 is 3rd
    barplot(py, names.arg = 0:5, main="Question 2a", xlab="y", col="lightblue")
    print(sum(py[3:5])) # P(2<=X<=4)
    
    #Q2b)
    prob_b = 4/10    
    b = dbinom(0:5, size = 5, prob = prob_b)
    print(sum(b[3:5])) # P(2<=X<=4)

    # Q2c)
    N = 100
    k = 40
    n = 5
    x = 0:5
    py = dhyper(x, k, N-k, n)
    print(sum(py[3:5])) # P(2<=X<=4)

    prob_b = k/N
    b = dbinom(x, size = n, prob = prob_b)
    print(sum(b[3:5])) # P(2<=X<=4)

    # Q2d)
    c_hyper = phyper(x, k, N-k, n)
    c_binom = pbinom(x, size=n, prob=k/N)

    plot(x, c_hyper, type = "s", col = "blue", lty = 2,
        main = "CDF: Hypergeometric vs Binomial",
        xlab = "Number of defectives", ylab = "CDF") # plot hypergeometric CDF

    lines(x, c_binom, type = "s", col = "red", lty = 1) # add binomial CDF

    legend("bottomright", legend = c("Hypergeometric", "Binomial"),
        col = c("blue", "red"), lty = c(2,1)) # so I can see which is which



    # Q3a)
    n = 20
    p_vals = seq(0.01, 0.2, by = 0.01)
    rejection_rate = numeric(length(p_vals)) # init

    for (i in 1:length(p_vals)) {
        p = p_vals[i]
        rejection_rate[i] = 1 - dbinom(0, size = n, prob = p)
    }
    rejection_rate

    plot(p_vals, rejection_rate, type = "b", pch = 19, col = "blue",
     xlab = "Actual % of Defectives (p)",
     ylab = "Rejection rate",
     main = "Rejection rate vs Actual % of Defectives")