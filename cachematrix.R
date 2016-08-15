## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
    
  get <- function() x
  setmatrixinv <- function(inverse) m <<- inverse
  getmatrixinv <- function() inv
  list(set = set, get = get, setmatrixinv = setmatrixinv, getmatrixinv = getmatrixinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatrixinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setmatrixinv(inv)
  inv
}
