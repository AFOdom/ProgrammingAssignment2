# Given a matrix, return a list of functions for getting and setting
# the inverse of that matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # sets x equal to the argument of "set()" and m to NULL at the parent
        # level, so the variables can be accessed by other child functions with
        # the same parent.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x # just returns x
        
        # assigns solve() function to m at the parent level, so m can be
        # used by other functions
        setinverse <- function(solve) m <<- solve 
        
        getinverse <- function() m # just returns m
        
        # puts all the functions we just created into a list and returns
        # that list to give other functions access to them
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# The x argument for cachesolve() should be the list of functions returned
# by makeCacheMatrix(). cachesolve() will return the inverse of the matrix
# cached in makeCacheMatrix().
cachesolve <- function(x, ...) {
       
        # get the inverse matrix using the getinverse() function
        # stored in x
        m <- x$getinverse()
        
        # if m is not null, let the user know the function is getting
        # cached data. Return the cached data. End the function.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
       
        # if m *is* null, assign the result of the get() function stored
        # in x to get the original matrix passed into makeCacheMatrix().
        data <- x$get()
        
        # get the inverse of that matrix
        m <- solve(data)
        
        # use the setinverse() function in x to set the variable
        # m to the inverse of data at the parent level, so all child functions
        # of this parent have access to its contents.
        x$setinverse(m)
        
        # return the inverse
        m
}