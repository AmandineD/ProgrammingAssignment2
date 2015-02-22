#1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # we set the matrix format 
        inverse <- NULL # by default, the result of inverse is set to be NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x # get the value of the matrix

        setinverse <- function(solve) inverse <<- solve # set the value of inverse, using the solve function
        getinverse <- function() inverse #get the value of inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
} 
 
#2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) { 
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
                #if the inverse has already been calculated, 
                #the cachesolve should retrieve the inverse from the cache.
        }
        data <- x$get()
        inverse <- solve(data, ...) #if not, we use the solve function again
        x$setinverse(inverse)
        inverse
}
