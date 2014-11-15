## The following two functions together yield the possibility to compute the inverse of a matrix in a way that the cache is accessed if the inverse is called for a second time. This way, the inverse only needs to be computed once which avoids time-consuming computations.  




## The following function creates the object "matrix".
makeCacheMatrix  <- function(x = matrix()) {
        I <- NULL ## initializes the variable for the inverse of x
        set <- function(y) {
                x <<- y
                I <<- NULL
        } ## definition of the "set" method which is used to set a new matrix
        get <- function() x ## definition of the "get" method which returns the value of the matrix
        setinverse <- function(inverse) I <<- inverse ## definition of the "setinverse" method which sets the inverse
        getinverse <- function() I ## definition of the "getinverse" method with returns the value of I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) ##returns all methods
}


## The following function returns a matrix that is the inverse of 'x'. If the inverse has already been computed before, cache is accessed.

cacheSolve <- function(x) {
	
        I <- x$getinverse() ## assigns the current value of the "inverse" to I - is either NULL or the actual inverse
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        } ## tests whether I already contains the inverse in which case nothing needs to be recomputed
        data <- x$get()
        I <- solve(data) ## computes the inverse
        x$setinverse(I) ## assigns the correct value of the inverse in the matrix object 
        I ## returns the inverse.
}

