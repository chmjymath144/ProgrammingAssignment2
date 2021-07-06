#This function is going to be used in creating a cache matrix

makeCacheMatrix <- function(x = matrix())
   {
    x <- NULL         #this is used for configuring the inverse making it as  NULL
    set <- function(y){
      x <<- y
      x <<- NULL
    }
    get <- function() {x}      
    setInverse <- function(inverse) {x <<- inverse}
    getInverse <- function() {x}
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

#This function is used to solve the cache of the matrix
cacheSolve <- function(x, ...)
  {
  if(!is.null(x)){                       
    message("get cached data")
    return(x)                       
  }
  mat <- x$get()
  inv <- solve(mat, ...) 
  x$setInverse(x)
  }

