## These functions together calculate the inverse of a matrix. 
## 

## this function creates a cache matrix for the CacheSolve function to reference back to

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
  set <- function(y){
  	x <<- y
  	m <<- NULL
}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
		list(set = set, get = get,
   		setinv = setinv,
   		getinv = getinv)
}


## This function calculates and caches the inverse of a matrix, referencing the above function for prior cached results.

cacheSolve <- function(x, ...) {
         m<-x$getinv()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinv(m)
    m
}
