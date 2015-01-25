## The CacheMatrix provides operations to put/get inverse of matrixs into/from cache.
## To put inverse matrix into cache : call makeCacheMatrix
## To get the cached inverse matrix : call cacheSolve


## The makeCacheMatrix computes the inverse matrix of input matrix x,
## and put the inverse matrix into cache.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function(){
		x
	}
	setInverse <- function(i){
		inverse <<- i
	}
	getInverse <- function(){
		inverse
	}
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
	
}


## The cacheSolve provides access to 
## cached inverse matrix of input matrix x if exists
## or call makeCacheMatrix to compute the inverse matrix and put in cache.
cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)){
		message("getting cached inverse")
		return (i)
	}
	data <- x$get()
	i <- solve(data)
	x$setInverse(i)
	i
    ## Return a matrix that is the inverse of 'x'
}

