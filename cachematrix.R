## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Following function creates a special matrix object that is really a list containing a function to 
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse (termed solve here in keeping with assignment terminology)
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL ## inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x

        setsolve <- function(inverse) i <<- inverse
        
        getsolve <- function () i
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function

## Following function computes the inverse of the special matrix created with the "makeCacheMatrix" function
## First it checks whether the inverse has already been computed and if yes, it retrieves the inverse from the cache  
## and skips computation. Else, it computes the inverse and sets it in the cache using the "setsolve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Check if inverse exists in cache for supplied matrix
        i <- x$getsolve()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## inverse does not exist, hence compute the same
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
