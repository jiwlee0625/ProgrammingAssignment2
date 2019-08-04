## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#This function creates a special "matrix" object that can cache its inverse.
# The first function, `makeVector` creates a special "vector", which is
# really a list containing a function to

# 1.  set the value of the vector -> set the value of the matrix
# 2.  get the value of the vector -> get the value of the matrix
# 3.  set the value of the mean - >  set the value of inverse
# 4.  get the value of the mean -> get the value of inverse

# <!-- -->

#     makeVector <- function(x = numeric()) {
#             m <- NULL
#             set <- function(y) {
#                     x <<- y
#                     m <<- NULL
#             }
#             get <- function() x
#             setmean <- function(mean) m <<- mean
#             getmean <- function() m
#             list(set = set, get = get,
#                  setmean = setmean,
#                  getmean = getmean)
#     }
# }

## Write a short comment describing this function
		m <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) m <<- inverse
		getinverse <- function() m
		list(set=set, get=get,
			setinverse=setinverse,
			getinverse=getinverse)
}
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
        # If the inverse has already been calculated (and the matrix has not changed),
        # 		then `cacheSolve` should retrieve the inverse from the cache.
		m <- x$getinverse()
		if(!is.null(m)) {
				message("getting cached data")
				return(m)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		return(m)
#cachemean <- function(x, ...) {
#     m <- x$getmean()
#     if(!is.null(m)) {
#               message("getting cached data")
#               return(m)
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m
#}
}
