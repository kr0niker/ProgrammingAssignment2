
## I am using the function ginv() from the "MASS" package that returns the inversed matrix
## The functions allows to make a matrix, to get the matrix, to set its inverse and to get its inverse 
## (analoquosly to the function from the example)

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinv <- function(inv) m <<- inv
                getinv <- function() m
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        
}


## This function checks if the inversed matrix is already set and stored in cache, if so, returns it, if not, computes and returns it

cacheSolve <- function(x, ...) {
                m <- x$getinv()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- ginv(data, ...)
                x$setinv(m)
                m

}
