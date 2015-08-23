## This two functions allows to create a special "matrix" that
## can cache its inverse

## Return a special "matrix" object that can
## cache its inverse. It really a list containing
## a function to set the value of the matrix, get
## the value of the matrix, set the value of the 
## inverse and get the value of the inverse

makeCacheMatrix <- function(mat = matrix()) {
	## 'mat' is a square matrix

	inv <- NULL
	set <- function(m) {
		mat <<- m
		inv <<- NULL
	}
	get <- function() mat
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Return the inverse of mat. If the inverse has already
## been calculated (and the matrix has not changed), then
## this function retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
      ## 'mat' is a special "matrix" returned by
	## makeCacheMatrix

	inverse <- mat$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	data <- mat$get()
	inverse <- solve(data, ...)
	mat$setinverse(inverse)
	inverse
}
