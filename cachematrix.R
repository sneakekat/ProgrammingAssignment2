## makeMatrix function creates inverse of square matrix
## and stores the result in makeCacheMatrix function for easy
## access

## Sets & gets value of matrix for easy access
## setsinverse & gets inverse already stored

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  #only runs if I reset vector after initialized
    x <<- y     #sets x in parent environment  
    m <<- NULL  #sets m in parent environment
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse #sets m in parent environment
  getinverse <- function() m 
  list(set=set, get=get, setinverse=setinverse, 
       getinverse=getinverse)
  
}

## Calculates Inverse of square matrix
## Caches result in makeCacheMatrix function

cacheSolve <- function(x, ...) {   #still not sure why you need ...
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inverse")
    return(m)  #why do i need return? can't I just say m?
  }
  data <- x$get()
  m <- solve(data, ...) # was is ... for?
  x$setinverse(m)
  m
}