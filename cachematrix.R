#Background on the function
# makeCacheMatrix function retrieves and stores the value of 
# matrices and its inverse(only invertible matrices) and cacheSolve
# function checks whether the value of inverse is NULL or not and if it 
# is NULL then inverse is calculated and stored in the cached variable.

##This function stores an invertible matrix and its inverse.
##It only stores and gets matrices and does not calculate its 
##inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
      x <<- y
      i <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##The main purpose of this function is to check whether the cached
##value of inverse is NULL or not and if it is NULL then inverse is
## calculated and stored in the cached variable i.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
