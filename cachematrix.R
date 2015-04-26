## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverx <- NULL
    set <- function(y) {
        x <<- y
        inverx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverx <<- inverse
	
    getinverse <- function() inverx
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
		## try to get the inverse from cache if it was calculate
		inverx <- x$getinverse()
		if(!is.null(inverx)) {
			message("Getting cached data.")
			return(inverx)
		}
		data <- x$get()
		inverx <- solve(data)
		x$setinverse(inverx)
		inverx ## Return a matrix that is the inverse of 'x'
}
