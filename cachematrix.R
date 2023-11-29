## This function get a square matrix as an input and creat a list as an output.
##The output list involves 2 elements about input matrix and 2 elements
##which are functions and will be used in second function. We can say that
## the recent elements are empty.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The output list of first function will be used as an input for "cacheSolve"
## and this function first check that if the inverse of the matrix calculated
## before. Then it calculate the inverse of the matrix and put it in the cache.

cacheSolve <- function(x, ...) {
  ##assign fourth element of input to "inv"
  inv <- x$getinverse()
  ## check if the inverse calculated before
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## assign the second element of the input to "data" which is the main matrix 
  data <- x$get()
  ##assign the calculated inverse of the matrix to "inv"
  inv <- solve(data, ...)
  #run the third element of the input which is a function to cache the inverse
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
       
}




