## Week 3 Assignment: Catching the inverse of a matrix


## makeCacheMatrix creates a special matrix object thay can catche its inverse

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m<<-solve
	getInverse <- function() m
	list(set=set, get=get,
		setInverse=setInverse,
		getInverse=getInverse)
}


## cacheSolve computes the inverse of the special matrix returned by makeCatheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the catche.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
        		message("reading inverted matrix from cathe")
        		return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m	
}

## Test run
x <- matrix(1:4, nrow=2, ncol=2)
xx <- makeCacheMatrix(x)
cacheSolve(xx)
cacheSolve(xx)

