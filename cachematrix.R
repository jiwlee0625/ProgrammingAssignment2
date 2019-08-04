## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
# It returns a list containing functions that...
# 		1. set the value of the matrix
# 		2. get the value of the matrix
# 		3. set the value of inverse
# 		4. get the value of inverse
### 1. set the value of the matrix
		m <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
### 2. get the value of the matrix
		get <- function() x
###	3. set the value of inverse
		setinverse <- function(inverse) m <<- inverse
### 4. get the value of inverse
		getinverse <- function() m
### returns list of functions
		list(set=set, get=get,
			setinverse=setinverse,
			getinverse=getinverse)
}
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
# If the inverse has already been calculated (and the matrix has not changed),
# 		then `cacheSolve` should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the `solve` function in R.
# For example, if `X` is a square invertible matrix, then `solve(X)` returns its inverse.

	# first, we check to see if the inverse has already been calculated
		#get the cached inverse
		m <- x$getinverse()
		#if m isn't null, it means there's a cached calculation/inverse value already stored.
		if(!is.null(m)) {
				message("getting cached data")
				#return cached data
				return(m)
		}
	# if we're on this line, it means the inverse hasn't been calculated, so we need to compute and then store it in the cache
		#get matrix value
		data <- x$get()
		#compute inverse using solve, as per direction
		m <- solve(data, ...)
		#store/set computation we got for inverse in the cache for later so we don't have to do it again
		x$setinverse(m)
		#returns inverse
		m
}
