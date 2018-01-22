#!/usr/bin/env Rscript

QS <- function(a, b, c) {
# QS (QuadraticSolver) solves quadratic equations
# QS returns a vector of three values err, x1, and x2
# err error values:
#		0 = success
#		1 = non numeric input given
#		2 = undefined division by zero
#		3 = no real roots
#		4 = only one root
#
  res <- list()
  for (val in c("err", "x1", "x2")) {
		res[[val]] <- NA
	}

	# Coefficients must be numeric
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(c)) {
		#cat("non-numeric input\n")
		res$err <- 1
		return(res)
  }

  # a must be greater than 0
  if (a == 0) {
		#cat("undefined: division by zero\n")
  	res$err <- 2
		return(res)
	}

	# Discriminant = b²-4ac
	d <- (b^2) - (4*a*c)

	# No real roots when d < 0
	if (d < 0) {
		#cat("No real roots \n")
		res$x1  <- ((-b) - (sqrt(d)))/(2*a)
		res$x2  <- ((-b) + (sqrt(d)))/(2*a)
		if (is.nan(res$x1) || is.nan(res$x2)) {
			res$err <- 3
		}
		return(res)
	}

	if (d == 0) {
		#cat("Only one root (x1 = x2): ", "\n")
		res$x1  <- ((-b)/(2*a))
		res$x2  <- ((-b)/(2*a))
		if (res$x1 == res$x2) {
			res$err <- 4
		}
		return(res)	
	}

	if (d > 0) {
		#cat("Two real roots\n")
		res$x1  <- ((-b) - (sqrt(d)))/(2*a)
		res$x2  <- ((-b) + (sqrt(d)))/(2*a)
		res$err <- 0 
		return(res)
	}

}


QSTest01 <- function() {
# TEST01: Tests whether the input values are numeric or not
# Case 1: non numeric a
	a <- "p"
	b <- 9
	c <- -5
	
	r <- QS(a, b, c)

	if (r$err == 1) {
		cat("TEST01: Case 1: PASSED\n")
	}

# Case 2: non numeric b
	a <- 3
	b <- "5"
	c <- 2

	r <- QS(a, b, c)

	if (r$err == 1) {
		cat("TEST01: Case 2: PASSED\n")
	}

# Case 3: non numeric c
	a <- 3
	b <- 5
	c <- "2"

	r <- QS(a, b, c)

	if (r$err == 1) {
		cat("TEST01: Case 3: PASSED\n")
	}
}

QSTest02 <- function() {
# TEST02 tests division by zero error
	a <- 0
	b <- 7
	c <- 8	

	r <- QS(a, b, c)

	if (r$err == 2) {
		cat("TEST02: PASSED\n")
	}
}

QSTest03 <- function() {
# TEST03 tests no real roots return
	a <- 2
	b <- 2
	c <- 5

	r <- QS(a, b, c)

	if (r$err == 3) {
		cat("TEST02: PASSED\n")
	}
}

QSTest04 <- function() {
# TEST04 tests only one real root return
	a <- 3
	b <- 0
	c <- 0

	r <- QS(a, b, c)

	if (r$err == 4) {
		cat("TEST04: PASSED\n")
	}
}


