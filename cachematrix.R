## Put comments here that give an overall description of what your
## functions do

## The following function creates a matrix (e.g. makeCacheMatrix(matrix(1:4, 2, 2))).
## The respective inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
		m_inv <- NULL
		set <- function(y) {
				x <<- y
				m_inv <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) m_inv <<- inverse
		getinverse <- function() m_inv
		list(set = set, get = get, setinverse = setinverse, 
							getinverse = getinverse)
}


## The following function calculates the inverse of the matrix 
## & can store it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		m_inv <- x$getinverse()
		if(!is.null(m_inv)) {
				message("getting cached data")
				return(m_inv)
}
		data <- x$get()
		m_inv <- solve(data, ...)
		x$setinverse(m_inv)
		m_inv
}

