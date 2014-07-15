## These functions allow not to repeat matrix inversion - which can last long -
## caching the resulting matrix of the first time the origin matrix was inverted.

## The function makeCacheMatrix takes as argument any matrix and returns a list. 

## The function cacheSolve takes the result of makeCacheMatrix as argument and 
## returns the inverted matrix. Note that no control is done wether the matrix 
## is singular or not and, depending on the original matrix, an error can be 
## returned.

## 1st step, creating the cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
	
	## initialize the inverted matrix
	m <- NULL
	
	## define the set function attached to the cacheable matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## define the get function attached to the cacheable matrix
	get <- function() x
	
	## define the function to be used if the matrix wasn't yet inverted
	setinv <- function(solve) m <<- solve
	
	## define the function to be used if the matrix was already inverted
	getinv <- function() m
	
	## returns the cacheable matrix as a list
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}


## 2nd step inverting the matrix

cacheSolve <- function(x, ...) {
        ## tries to get the preexisting inverted matrix
	m <- x$getinv()
	
	## if it exists, then signals that a cache exists, returns the cached 
	## inverted matrix and exits
	if(!is.null(m)) {
		message("Matrix already inverted, getting cached data")
		return(m)
	}
	
	## the inverted matrix doesn't exist, take the origin matrix
	data <- x$get()
	
	## inverts it
	m <- solve(data)
	
	## stores the inverted matrix
	x$setinv(m)
	
	## and returns the result of the inversion
	m
}
