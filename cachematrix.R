## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  p_inversedMatrix <- NULL ## this variable stores the value of inversed matrix
  
  ## set the value of the matrix
  set <- function(matr){
    x <<- matr
    p_inversedMatrix <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse matrix
  setInverseMatrix <- function(inverseMatrix) {
    p_inversedMatrix <<- inverseMatrix
  }
  
  ## get the value of the inverse matrix
  getInversedMatrix <- function() p_inversedMatrix
  
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, 
       getInversedMatrix = getInversedMatrix)
}


## Return a matrix that is the inverse of 'x'.
## cacheSolve function calculates the inverse matrix of the special "vector" created using makeCacheMatrix function. 
## However, it first checks to see if the inverse matrix has already been calculated. If so, it gets the inverse matrix from the cache 
## and skips the computation. Otherwise, it calculates the inverse matri and sets it in the cache via the setInverseMatrix function.

cacheSolve <- function(x, ...) {  
  p_inversedMatrix <- x$getInversedMatrix()  # get the value of inversed matrix
  
  ## don't make inverse matrix calculation and return its value from cache if p_inversedMatrix is not NULL - meaning cached value exist
  if (!is.null(p_inversedMatrix)) {
    message("getting cached inversed matrix")
    return(p_inversedMatrix)
  }
  
  ## otherwise, calculate inverse matrix value and set it in cache
  message("calculating inversed matrix")
  data <- x$get() ## get matrix data for calculation
  p_inversedMatrix <- solve(data, ...) ## calculate inverse matrix
  x$setInverseMatrix(p_inversedMatrix) ## set calculated inverse matrix in cache
  
  p_inversedMatrix
}