## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function
## will solve inversible matrix and cache result by calling cacheSolve function



makeCacheMatrix <- function(x = matrix()) {
    ## is like returning class object that has four public method
    ## getter and setter for each of two private variable x and inv
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    
    if (!is.null(m)) {
        message('getting cached data')
        return(m)
    }
  
    data <- x$get()
    m <- solve(data)
    
    x$setinverse(m)
    ## returns inverse of matrix x
    m    
    
}
