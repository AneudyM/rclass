#!/usr/bin/env Rscript

# Variable initialization
a <- 1
b <- 5
c <- 8
d <- (b^2) - (4*a*c)

# Evaluation of input variables

# A must not be zero 
if (a == 0) {
  stop("undefined: division by zero. Variable 'a' must not be zero")
}

# Discriminant rules
if (d == 0) {
  # Only one root
  x <- (-(b)) / (2*(a)) 
  cat("Only one root: ", x, "\n")
} else if (d > 0) {
  # Two Real roots
  xpos <- (-(b) + sqrt(d)) / (2*(a))
  xneg <- (-(b) - sqrt(d)) / (2*(a))
  x <- c(xpos,xneg)
  cat("Two real roots: ", x, "\n")
} else if (d < 0) {
  # It has two complex roots
  cat("Two complex roots: NaNs \n")
}

# Test line
