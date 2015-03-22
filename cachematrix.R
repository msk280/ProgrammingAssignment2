## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function created a special Matrix object than can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and some associated sub-functions/methods define the cache m
  m <- NULL
  set <- function(y) {
    
    ## assign the input matrix y to the variable x in the parent environment
    x <<- y 
    
    ## re-initialize m in the parent environment to null
    m <<- NULL 
    
  }
  get <- function() x ## return the matrix x
  ## set the cache m equal to the inverse of the matrix x
  setinverse <- function(inverse) m <<- inverse 
  
  ## return the cached inverse of x
  getinverse <- function() m 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) 
  {
    m <- x$getinverse()
    
    if(!is.null(m)) 
    {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setinverse(m)
    m
  }
}
