## this script contains 2 functions that caching invert matrix by 
## using closure method from parent to child function
 

## set initial matrix value, getting matrix, setting inverse
## matrix and getting inverse matrix to be use on cacheSolve function

makeCacheMatrix <- function(mtrx = matrix()) {
  invm <- NULL
  
  setm <- function(y) {
    mtrx <<- y
    invm <<- NULL
  }
  
  getm <- function() mtrx
  setinv <- function(inv) invm <<- inv
  getinv <- function() invm
  list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
}

## check whether inverse matrix cached. if not, the value assign converted 
##to inverse matrix by using solve function

cacheSolve <- function(myfun, ...) {
  
  invm <- myfun$getinv()
  if(!is.null(invm)) {
    message("Invert Matrix already exist.")
    return(invm)
  }
  
  data <- myfun$getm()
  invm <- solve(data)
  myfun$setinv(invm)
  invm

}
