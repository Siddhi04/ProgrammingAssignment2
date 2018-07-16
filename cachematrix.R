makeCacheMatrix <- function(x = matrix()) 
{
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



#The following function calculates the mean of the special "matrix" created with the above function makeCacheMatrix. 
#However, it first checks to see if the mean has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) 
{
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) #checks in cache data
  {
    message("getting cached data") #if present prints the message.
    return(inv) #returns the cached data without computing further
  }
  
  data <- x$get() #if not available computes the calculation
  inv <- solve(data, ...)
  
  x$setInverse(inv)
  inv  #returns the calculated call
}
