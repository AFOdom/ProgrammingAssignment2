## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        #inverse <- solve(x)
        get <- function() x
        setInverse <- function(inverse) m <<- solve(x)
        list (get=get,solvedMatrix = inverse, setInverse = setInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$solvedMatrix
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get
        print(data)
        inverse <- solve(data)
        print(paste("inverse is",inverse, sep=""))
        x$setInverse(inverse)
        inverse
}
