## makeCacheMatrix and cacheSolve are functions created to help work with large matrices. 
## Calculating inverses of matrices is time consuming, and the two functions help reduce that time.
## makeCacheMatrix creates a matrix that saves information about it's inverse, if it has been calculated. 
## cacheSolve returns the inverse of a matrix, retrieving it from the cache, if it's been calculated previously,
## otherwise it calculates the inverse, returns it and saves it in the cache.


## makeCacheMatrix is a function that creates a special "matrix", which is really a list
## containing a function to
## 1. set: sets the value of the matrix
## 2. get: gets the value of the matrix
## 3. setInverse: sets the value of the inverse of the matrix
## 4. getInverse: gets the value of the inverse of the matrix

## makeCacheMatrix takes an input x, that needs to be an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # inverse stores the inverse of the matrix x
  inverse <- NULL
  
  ## 1. set: sets the value of the matrix and
  ##         resets the inverse to null
  set <- function(y){
    x <<- y
    # reset inverse
    inverse <<- NULL
  }
  
  ## 2. get: gets the value of the matrix
  get <- function() x
  
  ## 3. setInverse: sets the value of the inverse of the matrix
  setInverse <- function(inv) inverse <<- inv
  
  ## 4. getInverse: gets the value of the inverse of the matrix
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
