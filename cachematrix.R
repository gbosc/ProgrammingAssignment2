############################################################################
## Course: R Programming
## Week 3: Assignment
##         The goal of this assignment is to write a pair of functions that
##         cache the inverse of a matrix
## Assumptions: Assume that the matrix supplied is always invertible
##              Assume that the matrix supplied is square.
##
## To understand what the inverse of a matrix is, go to this website:
## https://www.mathsisfun.com/algebra/matrix-inverse.html
############################################################################

## The makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix function above. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if (!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}

########################################################################################################
## Testing the makeCacheMatrix and cacheSolve functions
##
## Step 1: Create a matrix that is square (a matrix with the same number of rows and columns)
##         and invertible.
##
## > x <- matrix(c(1,1,1,0), 2, 2)
##
## Step 2: List out the content of x
##
## > x
##      [,1] [,2]
## [1,]    1    1
## [2,]    1    0
##
## Step 3: Confirm that the matrix is invertible. This is done using the determinant function.
##         If the result is not zero, then the matrix is invertible.
##
## > det(x)
## [1] -1
##
## Step 4: Using the makeCacheMatrix function, prepare the 's' object.
##
## > s = makeCacheMatrix(x)
##
## Step 5: Confirm that the previous function call worked, retrieve x from s using the get() function
##
## > s$get()
##      [,1] [,2]
## [1,]    1    1
## [2,]    1    0
##
## Step 6: Call the cacheSolve function for the first time (nothing in the cache the 1st time)
##
## > cacheSolve(s)
##      [,1] [,2]
## [1,]    0    1
## [2,]    1   -1
##
## Step 7: Call the cacheSolve function a second time (now there should be something in the cache)
##
## > cacheSolve(s)
## getting cached data
##      [,1] [,2]
## [1,]    0    1
## [2,]    1   -1
########################################################################################################
