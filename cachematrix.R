## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
