## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                    ## Init the Matrix for holding matrix inverse
    set <- function(y){                            ## Set function to assign few value of matrix assignment
      x <<- y
      inv <<- NULL                                 ## For new matrix reset inv to null
    }
    get <- function() x                           ## get funciton returns value of the matrix argument
    
    setInverse <- function(solveMatrix) inv <<- solveMatrix    ## assigns value of inverse matrix
    
    getInverse <- function() inv                  ## Create inverse of the matrix
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  ##Reference List
 }



## Write a short comment describing this function

## Ffunction to dervice the inverse of the matrix  returned by makeCacheMatrix as defined by the previous function
## If the inverse has  been derviced  and no change to matrix  
## the function cacheSolve will retrieve the inverse from the cache and will not derivce again
  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()    
  
  if(!is.null(inv)){
    message("getting cached data")               ## If the inverse has been already derviced get it from Cache
    return(inv)
  }
  
  data <- x$get()                               ## if not calculate the inverse 
  inv <- solve(data)
  x$setInverse(inv)                             ## Sets inverse of the cache
  inv                                           ## Return inverse 

  }
