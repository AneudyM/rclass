#!/usr/bin/env Rscript


QS <- function(a, b, c) {
# QS (QuadraticSolver) solves quadratic equations
# QS returns a vector of three values err, x1, and x2
# err error values:
#	    0 = success
#		  1 = non numeric input given
#		  2 = undefined division by zero
#		  3 = no real roots
#		  4 = only one root
#
  
  # Create return list of NAs
  res <- list()
  for (val in c("err", "x1", "x2")) {
    res[[val]] <- NA
  }
  
  # Coefficients must be numeric
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(c)) {
    res$err <- 1
    return(res)
  }
  
  # Undefined division by zero
  if (a == 0) {
    res$err <- 2
    return(res)
  }
  
  # Discriminant = b²-4ac
  d <- (b * b) - (4 * a * c)
  
  # No real roots when d < 0
  if (d < 0) {
    res$err <- 3
    return(res)
  }
  
  # Only one root
  if (d == 0) {
    res$err <- 4
    return(res)
  }
  
  # Two real roots
  if (d > 0) {
    res$x1  <- ((-b) - (sqrt(d))) / (2 * a)
    res$x2  <- ((-b) + (sqrt(d))) / (2 * a)
    res$err <- 0
    return(res)
  }
}


QSTest <- function() {
# TEST01: Tests whether the input values are numeric or not
  
  # Test cases 1-3: when non numeric input is given, QS should
  # return an error code of 1. If error code is 1, then test PASSED
  # otherwise FAILED.
  
  # TEST01 Case 1: non numeric a
  a <- "p"
  b <- 9
  c <- -5
  
  r <- QS(a, b, c)
  
  if (r$err != 1) {
    cat("TEST01: Case 1: FAILED\n")
  }
  
  # TEST01 Case 2: non numeric b
  a <- 3
  b <- "5"
  c <- 2
  
  r <- QS(a, b, c)
  
  if (r$err != 1) {
    cat("TEST01: Case 2: FAILED\n")
  }
  
  # TEST01 Case 3: non numeric c
  a <- 3
  b <- 5
  c <- "2"
  
  r <- QS(a, b, c)
  
  if (r$err != 1) {
    cat("TEST01: Case 3: FAILED\n")
  }
  
  # TEST02 tests division by zero error
  
  # Whenever a division by zero occurs QS should return
  # an error code of 2, otherwise FAILED is returned
  a <- 0
  b <- 7
  c <- 8
  
  r <- QS(a, b, c)
  
  if (r$err != 2) {
    cat("TEST02: FAILED\n")
  }

  # TEST03 tests no real roots return
  # FAILED if error code not equals 3
  a <- 2
  b <- 2
  c <- 5
  
  r <- QS(a, b, c)
  
  if (r$err != 3) {
    cat("TEST02: FAILED\n")
  }

  # TEST04 tests only one real root return
  # FAILED if error code not equals 4
  a <- 3
  b <- 0
  c <- 0
  
  r <- QS(a, b, c)
  
  if (r$err != 4) {
    cat("TEST04: FAILED\n")
  }
}
