## Caching the inverse of a matrix

## Creates a special matrix type

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y)
	{
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) inverse <<- solve
	getSolve <- function() inverse
	list(get = get, getSolve = getSolve, set = set, setSolve = setSolve)
}


## Computes de inverse of a matrix type returned by makeCacheMatrix. If the inverse has been calculated, retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getSolve()
	if(!is.null(inverse))
	{
		message("getting inverse")
		return (inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setSolve(inverse)
	inverse
}
