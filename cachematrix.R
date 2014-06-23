## This method creates a special matrix which allows caching the inverse matrix.
# It is is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL # create a NULL inverse matrix
	set <- function(y) { # set the matrix to be x, and its inverse to be NULL
		x <<- y
		xinv <<- NULL
	}
	get <- function () x # returns the matrix x
	setinverse <- function(inv) xinv <<- inv # set the inverse matrix
	getinverse <- function() xinv # returns the inverse matrix
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse) # list all functions, essential if we want to use them elsewhere
}


## This method searches if the inverse matrix is cached
# if not, the method computes the inverse and caches it
cacheSolve <- function(x, ...) {
	xMatrix <- x$get() # gets the matrix x and its dimensions
	nCol = ncol(xMatrix)
	nRow = nrow(xMatrix)
	if(nCol != nRow) { # if it is not square it returns an error
		return(message("fail: matrix should be square"))
	}
	xInverse <- x$getinverse()
	if(!is.null(xInverse)) { # searches for its inverse on the cache and it returns it
		message("getting cached data: \n\tif you changed the parameters, you should clean the cache with cacheClean(matrix)")
		return(xInverse)
	}
	#if it does not find it, it computes the inverse and caches it
	xInverse <- solve(xMatrix, ...)
	x$setinverse(xInverse)
	xInverse
}

## Cleans the cache
# the inverse can be computed using different parameters, but those are not kept on cache,
# and therefore the method cacheSolve will return the inverse matrix with the old parameters
# If we clean the cache, this will not happen
cacheClean <- function(x) {
	if(is.null(x$getinverse())) { # if it does not find an inverse there is nothing to do
		return(message("already clean"))
	}
	x$setinverse(NULL)
	message("cleaning cached data")
}
