## makeCacheMatrix is the foundation of the use of cacheSolve using lexical scoping
## It builds a list of 4 functions that are available in the parent environment:
##     1) set() assigns the input matrix to x and clears i in the parent environment 
##     2) get () getter function for matrix x
##     3) setinverse() setter function for inverse matrix in parent environment
##     4) getinverse() getter function for inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve uses functions defined in makeCacheMatrix and stored in list
## 1) read in cached inverse matrix
## 2) if matrix hasn't changed, return cached inverse matrix
## 3) If matrix has changed, calculate inverse matrix, update cache and return inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}
