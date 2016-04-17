## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  # Provides a default for m if cacheSolve has not yet been used when the global cache is run for the first time
  m <- NULL 
  set <- function(y) 
  {
    x <<- y # Cache the input matrix by setting to the global environment variable
    m <<- NULL
  }
  
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  
  list(set=set, get=get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix() # Accessing the variable x that is in the global environment (as opposed to the calling environment)
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }

  # If the cache returns nothing, proceed to solve the inverse of the matrix  
  matrix<-x$get()
  m <- solve(matrix, ...)
  
  x$setMatrix(m)
  
  m
}
