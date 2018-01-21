#!/usr/bin/env Rscript

# VARIABLE DECLARATIONS


args <- commandArgs(trailingOnly = TRUE)
values <- c(args[1], args[2], args[3])
cat(values)


QuadraticSolver <- function() {
# QuadraticSolver solves quadratic esquations

	# division by zero returns undefined
	if (a == 0) {
		stop("error: division by zero: undefined")
	}

	# discriminant d = (b² - 4ac)
	d <- (b^2) - (4*a*c)
	
	if (d < 0) {
		cat("No real solutions")	
	}


}

TestQS <- function() {
# insert unit test implementationn code
}
