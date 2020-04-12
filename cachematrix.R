## Caching the inverse of a matrix. As looping costs computation time, below two functions will cache the inverse of a matrix by taking advantage of scoping rules.

## makeCacheMatrix: to create a special matrix object, so that it can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	matinv <- NULL
	set <- function(y) {
		x <<- y
		matinv <<- NULL
}
	get <- function() x
	setinv <- function(inverse) matinv <<- inverse
	getinv <- function() matinv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve: computes the inverse of the original matrix input in the above makeCacheMatrix. Also, if the inverse is already present, then it will skip the computation and get the inverse directly from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	matinv <-x$getinv()
	if(!is.null(matinv)) {
		message ("getting cached data")
		return(matinv)
}
	dat <-x$get()
	matinv <- solve(dat, ...)
	x$setinv(matinv)
	matinv
}

