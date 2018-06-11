## Put comments here that give an overall description of what your
## functions do

## store inverse matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## find inverse of matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv) {
    message("getting cached data")
    return(inv)
  }
  my_data <- x$get()
  inv <- solve(my_data, ...)
  x$setinv(inv)
  inv
  
}
