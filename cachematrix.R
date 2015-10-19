## This functions caches the invers of the matrix 2X2. It has two functions, makeCacheMatrix
## cacheSolve

## makeCacheMatrix, creates a special matrix, with default number of rows of 2  and
## default number of columns 2 (it is possible for user to change number of rows and 
## columns

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y,r=2,c=2) {
                x <<- matrix(y,nrow=r, ncol=c)
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
##  If the inverse has already been calculated, it retrives inverse matrix from the cash

cacheSolve <- function(x, ...) {
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

