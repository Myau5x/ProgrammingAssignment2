## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix$s -- variable for inverse matrix
## makeCacheMatrix$get -- return original matrix, 
## makeCacheMatrix.getsolve -- return inverse matrix if it's already exist or NULL if not
## makeCacheMatrix.setsolve --- set s inverse matrix

## cashSolve compute inverse matrix if its not exist
## Argument of cashSolve should be this special "matrix" that created by function makeCacheMatrix
## First it try to get cached inverse matrix,if it is NULL it compute inverse.

## Write a short comment describing this function
## if x - original matrix, lets d<-makeCacheMatrix(x) then d.getsolve() - inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## cashSolve compute inverse matrix if its not exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}
