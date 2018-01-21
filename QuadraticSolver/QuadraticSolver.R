#!/usr/bin/env Rscript

QS <- function(values) {
# QS (QuadraticSolver) solves quadratic equations 
  cat("Enter the coefficients for a, b, and c: ")
	a <- as.numeric(readline("a = "))
	b <- as.numeric(readline("b = "))
	c <- as.numeric(readline("c = "))

	cat("You entered ", a, b, c)
	
}
