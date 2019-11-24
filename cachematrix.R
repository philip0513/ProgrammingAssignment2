## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {    ## define the argument with default mode of "matrix"
  inv <- NULL                                  ## initialize inv as NULL
  set <- function(y){                          ## define the set function to assign new value of matrix in parent environment
    x <<-y                                     
    inv <<- NULL                               ## reset inv to NULL if there is a new matrix
  }
  get <- function() x                          ## define the get function
  setinv <-function(invert) inv <<-invert      ##assign value of inv in parent4 enviornment
  getinv <-function() inv                      ##gets the value of inv 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
