## The following functions check if there is cached result for inverse of the matrix. If yes, then the    
## previously cached inverse is printed. If the inverse is not cached, then the inverse is calculated and cached.

## Takes matrix as an input and can cache inverse of the same matrix. Returns list of functions. 

makeCacheMatrix <- function(x = matrix()) { ## x is a function argument
  
  inv = NULL    ## set inv to NULL
  set = function(y) { ## set is a function that when called will
    x <<- y           ## set x in the parent environment as y
    inv <<- NULL      ## set inv to NULL
  }
  get = function() x  ## get is a function that when called will return x
  setinv = function(inverse) inv <<- inverse  ##setinv is a function that takes inverse of matrix as an argument
  ## and when called will cache the inverse as "inv" in the parent environment
  getinv = function() inv ##getinv is a function that returns inverse of matrix if one is cached
  list(set=set, get=get, setinv=setinv, getinv=getinv) ## the makeCacheMatrix will return a list of funcions when
  ## when called
}

## Solves cache of a matrix if it cannot get it from makeCacheMatrix

cacheSolve <- function(x) { ## initializes function that takes an object returned by makeCacheMatrix as an argument
  inv = x$getinv() ## calls getinv() function. Tries to retrieve inverse of matrix which was cached 
  ##  by makeCacheMatrix
  if (!is.null(inv)){ ## checks if the variableobject inv is null
    message("getting cached data") ##if it is not, gives a message
    return(inv) ## and returns the inverse of the matrix
  }
  data = x$get() ## if the inverse was null, then calls get() function to get the matrix that is cached in
  ## makeCacheMatrix environment
  inv = solve(data) ##solves inverse of the the matrix cached in data object 
  x$setinv(inv) ## calls setinv() function to cahce the result to the object originally created by makeCacheMatrix
  return(inv) #returns the inverse of matrix
}