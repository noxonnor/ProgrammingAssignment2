## The purpose of these functions is to enable caching of potentially time consuming computations, in this case the caching of inversing a matrix. 


## The first function creates a special "matrix" object that can cache its inverse. It's a list of four functions. 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  ## creates m with NULL value
  m <- NULL
  ## defines the set function, which takes a parameter y and stores it in global x. Also sets global m to NULL.
  set <- function(y){
    x<<- y
    m<<- NULL
  }
  ## defines the get function, which is a function that returns global x.
  get <-function() x
  ## defines the setinverse function, which is a function which takes in a matrix, and stores it to global m.
  setinverse <- function(solve) m <<- solve
  ## defines the getinverse function, which is a function that returns global m.
  getinverse <- function() m
  ## creates a lust of the functions we've defined in this function.
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Computes the inverse. If the inverse has already been calculated, fetch the cached inverse matrix and return it. 
## If not, compute the inverse matrix, store the result in the cache and return it. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## The input to this function is a list of functions. 
  ## Fetch the result of the function "getinverse"
  m <- x$getinverse()
  ## If the result is NOT empty, return the result and print message that data has been cached. End run.
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Assumes result IS empty, then fetch the result of the function "get", which gets the data of the matrix. 
  data <- x$get()
  ## Call the function Solve to inverse the matrix
  m <- solve(data, ...)
  ## Call the function "setinverse" to store the data as cache
  x$setinverse(m)
  ## Return the data
  return(m)
}
