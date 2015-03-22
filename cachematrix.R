## Here, we create a pair of function that can cache the inverse of the matrix

## makeCache creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	set_inverse <- function(solve) m <<- solve
	get_inverse <- function() m
	list(set = set, get = get,
		 set_inverse = set_inverse,
		 get_inverse = get_inverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse was already calculated, cacheSolve will just retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	inv_matrix <- x$get_inverse()
	if (!is.null(inv_matrix)) {
		message('getting inversed matrix from cached data')
		return(inv_matrix)
	}
	
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$set_inverse(m)
	m
}
