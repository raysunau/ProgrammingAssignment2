## This function creates a special "matrix" object that can cache its inverse.
## This function has 1 input matrix x and return a list of functions.     

makeCacheMatrix <- function(x = matrix()) {
    # define the variable m and assing null to it.
    m <- NULL
    
    # define a function "set" which assign y to the input matrix x and assign null to m.
    # "<<-" assign the variable in the global environment. 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # define a function to Return x which is the input matrix.
    get <- function() x
    
    # Assign the value of inverse to m.
    setinverse <- function(inverse) m <<- inverse
    
    # Return the result m. 
    getinverse <- function() m
    
    # return a list of 4 elements which are the functions defined above.
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##This function has 1 input which is the output of function makeCacheMatrix.

cacheSolve <- function(x, ...) {
    
    #call makeCacheMatrix's function "getinverse".
    m <- x$getinverse()
    
    #check whether m is calculated or not.
    if(!is.null(m)) {
        # m is not null which mean it has already calculated.
        message("getting cached data")
        # return the cached m.
        return(m)
    }
    
    # m is not calculated before and not in cache.
    # assign the input matrix of x to data.
    data <- x$get()
    
    # calculate the inverse of the input matrix and assign to m.
    m <- solve(data, ...)
    
    # call x's setinverse function and cache the result, 
    # so next time call cacheSolve function,
    # return the cached m in the if statement above,
    # dont need to calculate reverse again.
    x$setinverse(m)
    
    # return the result m.
    m
}

