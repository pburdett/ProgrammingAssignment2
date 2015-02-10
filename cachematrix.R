## Functions to calculate the inverse of (an invertible) matrix
## The function with return a cached solution if the calculation
## has already been computed; Else it will solve the inverse
## and return, and store the solution for future use

## makeCacheMatrix is a list of fucntions that create a list
## of functions to store the matrix, and its inverse once computed

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cahceSolve chceks to see if the inverse has already been computed
## If this is the case, it returns the cached value, else it will 
## calculate the inverse, store in the cache, and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinv(i)
	i

}
