## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i<- NULL
	set<- function(y){
		x<<- y
		i<<- NULL
	}
	get <- function() x
	setinv <- function(invs) i<<- invs
	getinv <-function() i
	list (set = set, get = get, 
		setinv = setinv,
		getinv = getinv)

}


## This function calculated the mean of the special "matrix" created with above ## function. Prints inverse from cache and if not in cache then calculates it,  ## then prints

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if(!is.null(i)){
		message("Getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinv(i)
	i

}
