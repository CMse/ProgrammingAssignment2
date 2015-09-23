## Class contains functions to provide a caching layer to matrix
## Once a matrix is "cached" by using closure makeCacheMatrix,
## it can be used to improve time-consuming operations like
## transpose 

## Function returns a vector containing following functions:
##     1. set. takes a matrix parameter and wraps itself around it
##     2. get. returns the matrix stored by this wrapper
##     3. settranspose. sets the transpose of the matrix
##     4. gettranspose. gets the cached transpose
makeCacheMatrix <- function(x = matrix()) {
	transpose <- NULL
	set <- function(y) {
		x <<- y
	}
	
	get <- function() x
	settranspose <- function(y) transpose <<- y
	gettranspose <- function() transpose
	list(	set = set,
		get = get,
		settranspose = settranspose,
		gettranspose = gettranspose)
}


## Function expects a closure of type makeCacheMatrix
## and returns the cached transpose of the matrix. If
## the transpose is not cached, the transpose is created
## and cached to speed up future calls
cacheSolve <- function(x, ...) {
	transpose <- x$gettranspose()
	if (!is.null(transpose)) {
		message("getting cached data")
	} else {
		message("transpose not cached, calculating")
		transpose<-solve(x$get())
		x$settranspose(transpose)
	}
	transpose
}
