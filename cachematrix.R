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
	
	i <- NULL	
	
	## cache the matrix, Null out the inverse as the matrix is new and likely different						
	set <- function(y) {
        x <<- y							
        i <<- NULL						
	}
	
	## fetch cached x from other environment
	get <- function() { x }		
	
	## cache it to i in other environment		
	setInverse <- function(inverse_matrix) {
        i <<- inverse_matrix			
    }

	## fetch cached i from other environment
	getInverse <- function() { i }		

	## return a list of functions
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## 
## Returns the inverse of matrix x
## Will first check the cache for the existing inverse and return that if found
## If not found, will calculate the inverse then cache it for the next call and return it
##
cacheSolve <- function(x, ...) {

	## get the cached inverse (might be null)
	cachedInverse <- x$getInverse()		
	if (!is.null(cachedInverse)) {		
		cachedInverse
	} else {
		xx <- x$get()					
		xxInverse <- solve(xx, ...)		
		x$setInverse(xxInverse)			
		xxInverse						
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
