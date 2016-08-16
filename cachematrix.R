## The makeCacheMatrix function creates a matrix object that 
##can cache the inverse of the matrix once it is found. The 
##cacheSolve function looks to see if the inverse has already 
##been found for the matrix and if it has returns that. if it 
##hasn't been cached yet it finds the inverse

## This function takes a matrix and allows the inverse to be cached

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


## Ths function checks to see if the inverse of a matrix has 
##already been cached if it has it retrieves it, if it hasn't it calculates it

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
