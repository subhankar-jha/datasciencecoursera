## makeCacheMatrix() and cacheSolve() are two functions which allow the user to
## input a matrix and calculate it's inverse 

## makeCacheMatrix is a user defined function which allows the user to create
## a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) x_inv <<- inverse
  get_inv <- function() m
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}



## cacheSolve() computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
  x_inv <- x$get_inv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  input_matrix <- x$get()
  x <- solve(input_matrix)
  x$setinv(x_inv)
  x_inv
}
