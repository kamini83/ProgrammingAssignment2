# This function creates a special "matrix" object that can cache its inverse.
# It assumes that the matrix supplied is always invertible i.e. no checks.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    #set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #get the value of the matrix
    get <- function() x

    #set the value of the inverse
    setinverse <- function(solve){
        message("calculating Ori Inverse")
        i <<- solve
        }

    #get the value of the inverse
    getinverse <- function() i

    #returns a list of the 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



# This function computes the inverse of the "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()

    #checks to see if an inverse has been stored for the given matrix
    if(!is.null(i)) {
        message("getting cached inversed data")
        return(i)
    }

    #calculates and stores the new inverse value if not found
    else{
        data <- x$get()
    i <- solve(data, ...)
    message("Inverse wasnt found - calculating New Inverse")
    x$setinverse(i)
    i
    }
}
