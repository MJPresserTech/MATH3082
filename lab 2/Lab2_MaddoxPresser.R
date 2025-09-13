# Lab 2 - Maddox Presser

# a)
n = 10
x = sample(0:1, 10, replace = TRUE)
print(x)

tot_heads = sum(x)
print(tot_heads)

# b)
arr = c(100, 500, 1000)
for (n in arr) {
  x = sample(0:1, n, replace = TRUE)
  tot_heads = sum(x)
  print(tot_heads)
}

# c)
par(mfrow = c(1, 2))
Nheads = cumsum(x)
plot(1:n, Nheads, type="o")

RelFreq = Nheads/(1:n) # this is "elementwise" division of two vectors
plot(1:n, RelFreq, type="o")
lines(c(1,n),c(0.5,0.5), col="red")

#d)
arr_n = c(10, 100, 500, 1000)

arr_prob = list(c(0.7, 0.3), c(0.3, 0.7), c(0.9, 0.1)) # list of probs
prob_labels = c("p=0.3", "p=0.7", "p=0.1")  # labels

par(mfrow = c(3, 4))
i = 1  # counter for labels
for (p in arr_prob) {
    p_heads = p[2]  # prob of heads
    
    for (n in arr_n) {
        x = sample(0:1, n, replace = TRUE, prob = p)
        tot_heads = sum(x)
        print(paste("Prob =", prob_labels[i], ", n =", n, ", heads =", tot_heads)) # answers
        
        Nheads = cumsum(x)
        RelFreq = Nheads/(1:n)
        
        plot(1:n, RelFreq, type="l", 
             main=paste(prob_labels[i], ", n =", n), # titles
             xlab="Flips", ylab="Rel. Freq.",
             ylim=c(0, 1), col="blue", lwd=2)
        lines(c(1,n), c(p_heads, p_heads), col="red", lwd=2) 
    }
    i = i + 1 # for next label
}

#e)

