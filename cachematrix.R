## Functions to create an object that can store a
## matrix and its inverse and return that inverse
## or calculate it

## function for creating an object which has a matrix 
## and holds an inverse for it, so that it only needs
## be calculated once  

makeCacheMatrix <- function(x = matrix()) {
 
## m - internal variable for matrix inverse
##   - null = not yet set
  m <- NULL
## set - function to give a value to X (input matrix)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
## get - function gets X value
  get <- function() x

## setinverse - function to give a value to m (matrix inverse)
##            - diag() gives identity matrix 
##            - solve(a,b) gives solution for a.? = b
  setinverse <- function() {
    m <<- solve(x,diag(dim(x)[1]))
    return(m)
  }

## getinverse - function to get value of m
  getinverse <- function() m

## return object of function
  list(
        set = set, 
        get = get, 
        setinverse = setinverse,
        getinverse = getinverse
        )
}


## Return a matrix that is the inverse of 'x'
## function for passing a makeCacheMatrix object and 
## calculating an inverse of the attached matrix
## if already calculated it will use this value
## otherwise it will calculate it

cacheSolve <- function(x, ...) {
  
## get inverse of x  
  mi <- x$getinverse()
## if it exists return the value and exit
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
## if not calculate the value and return
  mi <- x$setinverse()
  return(mi)
}
