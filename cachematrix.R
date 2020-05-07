## Caching the Inverse of the matrix:
## Matrix inversion is usually a costly computing type and there are some benifits of caching the inverse of matrix rather than computing it repeatedly
## The following pair of function stores the matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- null
	set <-function(y){
		x <<- y
		inv <<- null
	}
	get <- function()x
	setInverse <- function(Inverse) inv <<- Inverse
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the Inverse of the special "matrix" created with the above function.
## If the inverse is already calculated the inverse should be retrieved from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
      	inv<- solve(mat, ...)
        x$setInverse(inv)
        inv
}
