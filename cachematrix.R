## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       data <- x$get()
        m <- x$getinverse()
       if(identical(x, data) && !is.null(m)) {
                message("getting cached data")
                return(m)
       }
       
       ## Calculate inverse
       ## if x is square matrix, use solve()
       ## else use ginv() in the MASS package - with the assumption input matrix is inversible
       if(dim(data)[1] == dim(data)[2]){
       	m <- solve(data, ...)	
       }else{
       	m <- ginv(x)
       }
        
        x$set(x)
        x$setinverse(m)
        m

}
