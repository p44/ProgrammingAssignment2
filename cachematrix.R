## Assignment 2 - demonstrate caching results of expensive operations and using the cache

##
## This function creates a special "matrix" object that can cache its inverse.
## Do not doubt it's specialness.
## 
## Returns a list containing functions to:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse matrix
##   get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
	## define the functions
	i <- NULL							# default cached inverse matrix to Null
	set <- function(y) {
        x <<- y							# cache it to x in other environment
        i <<- NULL						# Null out the inverse matrix upon set (could be different)
	}
	get <- function() { x }				# fetch cached x from other environment
	setInverse <- function(inverse_matrix) {
        i <<- inverse_matrix			# cache it to i in other environment
    }
	getInverse <- function() { i }		# fetch cached i from other environment

	## return a list of functions
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## 
## Returns the inverse of matrix x
## Will first check the cache for the existing inverse and return that if found
## If not found, will calculate the inverse then cache it for the next call and return it
##
cacheSolve <- function(x, ...) {
	cachedInverse <- x$getInverse()		# get the cached inverse (might be null)
	if (!is.null(cachedInverse)) {		# if found in cache return it.
		cachedInverse
	} else {
		xx <- x$get()					# get the matrix to calc inverse
		xxInverse <- solve(xx, ...)		# calculate inverse
		x$setInverse(xxInverse)			# cache the inverse
		xxInverse						# return the inverse - next time will be much faster
	}
}

##
## To test - using the example from the solve() docs
## > hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## > h8 <- hilbert(8)
## > solve(h8)
## > mm <- makeCacheMatrix()
## > mm$set(h8)
## > mm$getInverse() # should be NULL
## > cacheSolve(mm)
## > cacheSolve(mm)
##
