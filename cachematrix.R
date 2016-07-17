## Data Scientist - R Programming - Week 4
## Programming Assignment 2: Lexical Scoping
## Deadline: 2016-07-17

## Objective: Caching the Inverse of a Matrix
## quoted from the assignment page:
## "Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse."

## Requirements: Create two functions

## Function #1 : makeCacheMatrix
## -> creates a "matrix" object
## -> cache its inverse upon function setInverse is called via the Function #2

makeCacheMatrix <- function(x = matrix()) {
	
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inversematrix) inv <<- inversematrix
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function #2 : cacheSolve
## -> get the matrix from Function #1, creates the inverse matrix using the solve function unless already in the cache
## -> return the inverse matrix

cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Cached data is available -- getting the inverse matrix from cache")
    return(inv)
  }
  mat <- x$get() # get the matrix
  inv <- solve(mat, ...) # inverse the matrix
  x$setInverse(inv) # save in cache
  inv
}

