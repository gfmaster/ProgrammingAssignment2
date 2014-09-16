## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function
## will solve inversible matrix and cache result by calling cacheSolve function



makeCacheMatrix <- function(x = matrix()) {
    ## is like returning class object that has four public method
    ## getter and setter for each of two private variable x and inv
    inv <- NULL
    
    ## setter and getter for x
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    
    
    ## setter and getter for inv (which holds inverse matrix of x)
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    ## returns list object with all the methods
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    
    ## if cached inverse matrix is there
    if (!is.null(m)) {
        message('getting cached data')  ## let user know it is cached version
        return(m)                       ## return the value and end function
    }
  
    ## if there is not
    
    ## get the original matrix and
    data <- x$get()
    m <- solve(data)      ## solve for inverse
    
    x$setinverse(m)       ## cache
    
    
    ## returns inverse of matrix x
    m    
    
}
