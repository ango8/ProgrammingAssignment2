## Put comments here that give an overall description of what your
## functions do
## Programmer: Amanda Ngo 
## cachematrix.R consists of two functions: makeCacheMatrix() and cacheSolve().  
## makeCacheMatrix() creates an R object that stores a matrix and its inverse. 
## cacheSolve() calculates the inverse of the matrix and requires an input    
## argument of type makeCacheMatrix() in order to get the inverse   
## from the cache.

## Write a short comment describing this function
## makeCacheMatrix() builds a set of functions that consist of set(), get(),
## setinverse() and getinverse() that are returned within a list to the 
## parent environment as a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the value of the inverse object. 
        i <- NULL
        
        ## Assign the 'y' object to the 'x' object and assign a NULL value
        ## to the 'i' object in order to clear any previous value of 'i' that
        ## had been cached when makeCacheMatrix() had been executed before. 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Get the value of the matrix.
        get <- function () x

        ## Assign the inverse input argument to the 'i' object.
        setinverse <- function (inv) i <<- inv
        
        ## Get the value of the cached inverse.
        getinverse <- function() i 
        
        ## Create a list with each of the functions as an element and
        ## return the object.
        list (set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve() calculates the inverse of the special "matrix" object that is 
## returned by makeCacheMatrix(). If the inverse has already been computed and 
## the input matrix has not changed, then cacheSolve() will get the inverse 
## from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the inverse from the input argument object.
        i <- x$getinverse()
        
        ## Check to see if there is a NULL value or a cached inverse value.
        ## If the condition !is.null(i) is true, the cached inverse is returned.
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        ## Otherwise, retrieve the matrix from the input argument object. 
        data <- x$get()
        ## Calculate the inverse of the matrix. 
        i <- solve(data, ...)
        ## Set the value of the inverse in the input argument object.
        x$setinverse(i)
        ## Return and print the inverse. 
        i       
}
