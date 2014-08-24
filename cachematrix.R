## This code caches the inverse of a given matrix.
## Using the solve() function, the inverse of a matrix is calculated, while the results are being cached.

## Create a new matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ##set value of matrix
                x <<- y
                m <<- NULL
        }
        
        get <- function() x ##get value of matrix
        
        setInverse <- function(solve) m <<- solve ##create inverse of matrix
        
        getInverse <- function() m ## get the result of the inverse matrix
        
        list(set = set, get = get, ## result is a list, which is returned
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## The inverse of the matrix created above is calculated.
## If the inverse of the matrix is already available, use the cache to return it.
## If not, inverse the matrix, use the cache to save the inverse and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()  ## get from cache
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m ## Return inverse matrix
}

