## This file returns inverse of a matrix either from cache or by computation
## If the given input matrix is identical to the cached version of the matrix, 
## then the inverse of the matrix is returned from cache.
## And if the given input matrix is not identical to the cached version of the matrix, 
## then the inverse of the matrix is computed and stored in cache.
## The following functions are used:
### 1) makeCacheMatrix
### 2) cacheSolve


## makeCacheMatrix(matrix)
## Brief Overview
### This function is a work-around to standard R matrix 
### giving valuable options such as matrix setter, matrix getter, 
### matrix inversion setter and matrix inversion getter.
## Input        : Matrix for which inverse is needed
## Assumptions  : Input matrix should be square (n-by-n) matrix and 
##                invertible (non-singular or non-degenerate) matrix
## Output       : A list of four elements is returned and stored in private environment
## Elements in the list returned as output of the function
### 1) setMatrix
### 2) getMatrix
### 3) setInverse
### 4) getInverse

makeCacheMatrix <- function(mat = matrix()) {
  
  ## Initialize private variable for inverse of a matrix
  matInverse <- NULL
  
  ## setMatrix(matrix)
  ## Sets the input matrix in private environment and 
  ## clears the value of matrix inversion variable from cache
  setMatrix <- function(matNew) {
    mat <<- matNew
    matInverse <<- NULL
  }
  
  ## getMatrix()
  ## Returns the input matrix stored in private enviroment
  getMatrix <- function() {
    mat
  }
  
  ## setInverse(matrix)
  ## Sets the value of the inverse of the input matrix
  ## in private environment
  ## Note : This does not compute matrix inversion!
  setInverse <- function(inverseNew) {
    matInverse <<- inverseNew
  }
  
  ## getInverse()
  ## Returns the cached value of inverse of the matrix
  ## Note : Returns NULL if the input matrix is modified
  ##        or inverse is not set
  getInverse <- function() {
    matInverse
  }
  
  ## Returns a list object comprising of setter and getter elements
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheSolve(CacheMatrix, ...)
## Brief Overview
### This function returns a matrix which is inverse of the cacheMatrix (m)
### either from cache (if found) or after computation of matrix inversion
## Input        : CacheMatrix object returned from makeCacheMatrix function and
##                Any additional arguments (should be specified explicitly) for solve function
## Output       : Inverse of the cacheMatrix (m)

cacheSolve <- function(m, ...) {
  
  ## Get the cached value of inverse of the matrix
  mInverse <- m$getInverse()
  
  ## Check if the inverse of matrix is present in cache
  if(!is.null(mInverse)) {
    ## Returns the inverse of matrix from cache and exits the cacheSolve function
    message("Getting cached data")
    return(mInverse)
  }
  
  ## Cache is empty i.e. inverse of matrix is outdated or never been computed
  ## Inverse of the matrix is Computated as below
  
  ## Initialize the new/modified matrix in private environment
  data <- m$getMatrix()
  
  ## Computes the inverse of the matrix
  mInverse <- solve(data, ...)
  
  ## Sets the computed inverse of matrix in cache for easy retrieval next time
  m$setInverse(mInverse)
  
  ## Returns the computed inverse of matrix
  mInverse
}
