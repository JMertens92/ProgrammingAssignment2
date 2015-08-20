##R programming- Assignment 2 
## 2 functions:  (1) makeCacheMatrix: to create a  special"matrix" object that can
##               cache its inverse and 
##               (2)cacheSolve:calculates or retrieves inverse of the special "matrix"

## makeCacheMatrix create the list that contains 4 functions
## set the value, get the value, set the inverse, get the inverse

makeCacheMatrix <- function(x = matrix())  {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
        

## returns the inverse of the matrix by either retrieving it from cache or calculating it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the original
        
        i <- x$getinv()
        if(!is.null(i)) {      ##first check to see if it is in cache    
                message("getting cached data")
                return(i)    ## if found in cache return inverse
        }
        data <- x$get()      ## if not found in cache 
        i <- solve(data, ...)   ## calulcate inverse and 
        x$setinv(i)                ## set the value in the cache matrix
        i                            ## return the value
}
