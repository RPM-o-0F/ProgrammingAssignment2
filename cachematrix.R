#These functions illustrate how the use of the super-assignment operator (<<-)
#can be used to utilize scoping rules in R functions.

# This function establishes the matrix and a list of functions that includes
# getting and setting the matrix and getting and setting the inverse of 
#the matrix.  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #initializes m
        set <- function(y) {
                x <<- y #assigns x in the calling environment
                m <<- NULL #assigns m in the calling environment
        }
        get <- function() x # auto print of x
        setinverse <- function(solve) m <<- solve # assigns m in calling env.
        getinverse <- function() m # auto print of m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#This function will take advantage of a previously solved inverse matrix 
#to save from solving for the inverse matrix unnecessarily 

cacheSolve <- function(x, ...) {
        m <- x$getinverse() #calls for existing value of inverse
        if(!is.null(m)) { #if a value exist, use it
                message("getting cached data")
                return(m)
        }
        data <- x$get() #if value doesnt exist then solve for inverse matrix
        m <- solve(data, ...)
        x$setinverse(m)
        m}
