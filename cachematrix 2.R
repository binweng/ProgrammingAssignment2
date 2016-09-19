## Put comments here that give an overall description of what your
## functions do

# Caching inverse of a matrix

## Write a short comment describing this function

# Creates a matrix that can cache the inverse

# input - A matrix
# output - a matrix with function to get/set value for matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #inverse of matrix
  inv <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  list(get=get, set=set, 
       getinv=getinv, 
       setinv=setinv)
}



## Write a short comment describing this function
#The following function calculates the inverse of a matrix 
#created with the above function. However, it first checks 
#to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  #get the matrix
  m <- x$get()
  
  # compute inverse of matrix 
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  inv
}
