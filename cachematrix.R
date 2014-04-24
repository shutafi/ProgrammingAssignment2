## The following functions are intended to help speed up computations
## of the matrix's inverse.  In order to speed up the computations
## a cache systems is employed.

## 'makeCacheMatrix' receives a normal matrix which will have additional
## functions added to help contain the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # 'inv' will store the inverse matrix
  
  ## basic set function to store the matrix and initialize 'inv'
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## 'get' function provides a way to access the matrix
  get <- function(){
    x
  } 
  
  ## 'setInverse' allows the 'inv' variable to be set in this object
  setInverse <- function(inverse){ 
    inv <<- inverse
  }
  
  ## 'getInverse' allows access to the 'inv' variable
  getInverse <- function(){
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## 'cacheSolve' function will test if the new matrix object has the inverse
## available if not it will create one and store it in the passed matrix object

cacheSolve <- function(x, ...) {
  ## Access matrix x's inverse variable
  inv <- x$getInverse()
  
  ## this if statement will check to see if the inverse value has already been
  ## calculated.  If it has already been calculated then it will return it
  ## else it will be computed and set in the new matrix object.
  if(!is.null(inv)){
    message("getting cached value")
    return(inv)
  } else {
    data <- x$get() ## access the matrix in x
    inv <- solve(data) ## computes the inverse of matrix x
    x$setInverse(inv)  ## stores the inverse in matrix x object
    inv
  }
}
