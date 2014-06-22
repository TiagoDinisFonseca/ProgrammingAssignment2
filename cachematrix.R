## This method creates a special matrix, which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix
#
makeCacheMatrix <- function(x = matrix()) {
	# create a NULL inverse matrix xinv
	xinv <- NULL
	# set the matrix x, and its inverse to be NULL
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	# returns the matrix x
	get <- function () x
	# set the inverse matrix of x
	setinverse <- function(inv) xinv <<- inv
	# returns the inverse matrix of x
	getinverse <- function() xinv
	# list all functions, essential if we want to use them elsewhere
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This method searches for the inverse matrix on the cache of x
# if it does not find it, the method computes the inverse and puts on the cache
cacheSolve <- function(x, ...) {
	# gets the matrix x and its dimensions
	xMatrix <- x$get()
	nCol = ncol(xMatrix)
	nRow = nrow(xMatrix)
	# if it is not square it returns an error
	if(nCol != nRow) { 
		return(message("fail: matrix should be square"))
	}
	# searches for its inverse on the cache
	# if it finds it return its value
	xInverse <- x$getinverse()
	if(!is.null(xInverse)) {
		message("getting cached data")
		return(xInverse)
	}
	#otherwise it computes the inverse and puts on the cache
	xInverse <- solve(xMatrix, ...)
	x$setinverse(xInverse)
	xInverse
}

## Cleans the cache
# the inverse can be computed using different parameters, but those are not kept on cache,
# and therefore the method cacheSolve will return the inverse matrix with the old parameters
# If we clean the cache, this will not happen
cacheClean <- function(x) {
	# if it does not find an inverse there is nothing to do
	if(is.null(x$getinverse())) {
		return(message("already clean"))
	}

	x$setinverse(NULL)
	message("cleaning cached data")
}
