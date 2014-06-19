## Two functions that enable providing the matrix inverse efficiently.
## Instead of computing the inverse everytime, the inverse is computed and cached
## in memory, and then returned from cache subsequently.

## makeCacheMatrix returns a list of functions that help in caching the inverse
## of a matrix. It takes a matrix as input for which the inverse has to be computed.
## The matrix itself is stored for future use.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		message("Setting data");
		x <<- y
		inv <<- NULL
	}

	get <- function() x

	setinv <- function(i) inv <<- i
	
	getinv <- function() inv
	
	func <- function () {
		flist <- list(set = set, get = get,
			setinv = setinv,
			getinv = getinv) 
		message("In function func");

		flist
	}
	flist <- func()
	flist
}


## cacheSolve returns the inverse of a matrix. It  checks if the inverse
## has been previously computed and stored in cache (by makeCacheMatrix)
## and if not computes and returns it. When the inverse is computed,
## the value is stored in cache through the setinv function provided
## by makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	data <- x$get()
	inv <- solve(data,...)
	
	x$setinv(inv)

	inv
}
