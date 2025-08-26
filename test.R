# test.R
# Example R code for testing basic functionality

# Print Hello World
test_hello <- function() {
  print("Hello, world!")
}

test_hello()

# Simple test: add two numbers
test_add <- function(a, b) {
  return(a + b)
}

result <- test_add(2, 3)
cat("2 + 3 =", result, "\n")

# Test if a number is even
test_is_even <- function(x) {
  return(x %% 2 == 0)
}

cat("Is 4 even?", test_is_even(4), "\n")
cat("Is 5 even?", test_is_even(5), "\n")
