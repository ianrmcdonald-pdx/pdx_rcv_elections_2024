# Recursive function to calculate factorial
calculate_factorial <- function(n) {
  # 1. Base Case: stop when n is 0 or 1
  if (n <= 1) {
    return(1)
  } else {
    # 2. Recursive Step: n * factorial of (n-1)
    return(n * calculate_factorial(n - 1))
  }
}

calculate_factorial(5) # Returns 120
