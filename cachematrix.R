## Function creates an square(nXn) matrix with getter and setter methods
## 
## Create a makeCacheMatrix object
##  cm <-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
##
## Verify get function returns 2X2 matrix
##  cm$get()
##        [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
##
## Verify inverse matrix return NULL
##  cm$getInverseMatrix()


makeCacheMatrix <- function(x = matrix()) {
  
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(m) im <<- m
  getInverseMatrix <- function() im
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## Function return the inverse of matrix passed as parameter.
## First time it computes the inverse and subsequent calls returns
## result from cache
## 
## First call returns
## cacheSolve(cm)
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
## 
## Subsequent call 
## cacheSolve(cm)
##  getting inverse matix cached data
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  im <- x$getInverseMatrix()
  if(!is.null(im)) {
    message("getting inverse matix cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverseMatrix(im)
  im
  
  
}
