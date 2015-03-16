## My statements about the functions are above them.
## Function makeCacheMatrix produces four different function in the last step,
## which will be used in the cacheSolve function. The four functions define:
## 1) set the value of the matrix, 2) get the value of the matrix, 3) set the
## inverse, 4) get the inverse value.
makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## use function get() to obtain the value of the matrix, it will be used 
        ## in the function "cacheSolve".
        get <- function()x
        ## set the value of the inverse
        setinverse <- function(inverse) m <<- inverse
        ## get the value of the inverse
        getinverse <- function() m
        ## list() will return a list with four functions, which will be called 
        ## by the cacheSolve function.
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}

## Function cacheSolve will call functions defined in makeCacheMatrix.
## It will use the called function to calculate inverse of the matirx and cache 
## the value.
cacheSolve <- function(x, ...){
        ## The following calls function getinverse(), which is defined in makeCacheMatrix
        ## and assigns it to m
        m <- x$getinverse()
        ## checks to see if the inverse has already been calculated
        if(!is.null(m)){
                ## If inverse already been calculate, it returns the inverse value
                ## from the cache and skips the computation, and produce the message
                message("getting cached data")
                return(m)
        }
        ## If the inverse haven't been calculate, it will calculates the inverse
        ## of the data. First it call get() function to obtain the value of the 
        ## matrix and assign it to data, then solve(data) calculates the inverse,
        ## and assigns as "m". It finally calls function setinverse() to cache the
        ## calculated value and return as m.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
