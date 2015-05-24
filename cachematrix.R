## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_o <- NULL
  set <- function(y) {
    x <<- y
    inv_o <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_o <<- solve
  getinv <- function() inv_o
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_o <- x$getinv()
  if(!is.null(inv_o)) { #if the inv was cached
    message("getting cached data")
    return(inv_o) # exit program without solving the subsequent code
  }
  data <- x$get()   #otherwise put the data n 'data'
  inv_o <- solve(data, ...) # compute the inverse 
  x$setinv(inv_o) # call the function to cache the inverse
  inv_o # return value
}
