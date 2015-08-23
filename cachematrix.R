## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {  
  inv = NULL         
  set = function(y) {                 
    x <<- y                
    inv <<- NULL         }       
  get = function() x         
  setinv = function(inverse) inv <<- inverse         
  getinv = function() inv       
  list(set=set, get=get, setinv=setinv, getinv=getinv) 
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){#check if inversed matrix already calculated
    message("getting cached data") # if so get matrix from cache
    return(inv)
  }
  mat.data = x$get()#if not calculate
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}



