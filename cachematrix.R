## Put comments here that give an overall description of what your
## functions do

## This function takes a square matrix as an input.
## The function returns a list with 4 elements storing get, set, getinv and setinv
## structure of the function.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
 }


## Function compute the inverse of the matrix.
## Input is the output of makeCacheMatrix - a square matrix
## Output is the inverse of the square matrix.
## If the calculated inverse already exists, it returns the inverse
## from cache, instead of recomputing it. 

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}

