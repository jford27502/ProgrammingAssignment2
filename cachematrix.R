##       1         2         3         4         5         6         7         8
## 45678901234567890123456789012345678901234567890123456789012345678901234567890
##
##	The following two functions, makeCacheMatrix() and cacheSolve(),
## 	implement a caching mechanism for fast retrieval of the inverse
## 	of a matrix.
##

##
## makeCacheMatrix
##
##   Parameter: A square invertible matrix
##
##   Returns:
##     A special "matrix" in the form of a list containing
##     the accessor functions:
##         set - Set the cached matrix
##         get - Get the cached matrix
##         setInverse - Set the inverse of the matrix previously passed to 
##                      this function or set via the 'set' function of the
##                      returned function list.     
##         getInverse - Get the inverse of the matrix as previously
##                      set by a call to 'setInverse'.
##

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(inMatrix) {
		x <<- inMatrix
                inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
        list(set = set,
	     get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}

## cacheSolve
##
##   Parameters:
##     x - Special "matrix" returned by makeCacheMatrix
##     ... - Optional additional parameters passed to solve()
##
##   Returns:  The inverse of x's matrix
##

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if (!is.null(inv)) return(inv)
	matrix <- x$get()
	inv <- solve(matrix, ...)
        x$setInverse(inv)
	inv
}
