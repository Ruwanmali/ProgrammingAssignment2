##Two functions are used to create a special object that stores a matrix and cache's its inverse. 
## This function creates a special "matrix" object that can cache the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL 
        setm <- function(y) 
        {
                x <<- y 
                i<<- NULL
        }
        getm<-function()
        {
                x
        }
        seti<-function(inverse)
        {
                i<-inverse
        }
        geti<-function()
        {
                i
        }
        list(setm= setm, getm = getm,seti = seti,geti = geti)
}



##cacheSolve calls functions stored in the special "matrix" returned by makeCacheMatrix. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.If the input is new, it calculates the inverse of the matrix and sets the inverse in the cache via the seti function.

cacheSolve <- function(x, ...) {
        i <- x$geti() 
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
                
        matrix <- x$geti()                               
        i <- solve(matrix, ...)
        x$seti(i)
        i## Return a matrix that is the inverse of 'x'
}
