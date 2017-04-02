## Put comments here that give an overall description of what your
## functions do

## 1. make matrix that is assigned to variable x 
## 2. sign ind to NULL

makeCacheMatrix <- function(x = matrix()) {
  ind <- NULL
  set <- function(y) {
  x <<- y
  ind <<- NULL
  }
  get <- function() x
  setinvM <- function(invM) ind <<- invM
  getinvM <- function() ind
  list(set = set,
       get = get,
       setinvM = setinvM,
       getinvM = getinvM)
}


## This function is to return the inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    ind <- x$getinvM()              
    if(!is.null(ind)) {           
        message("getting cached data")  
        return(ind)              
    }  ## if ind has already existed then return ind
        
    data <- x$get()           
    ind <- solve(data, ...)       
    x$setinvM(ind)            
    ind   ## print the inverse matrix otherwise 
}
