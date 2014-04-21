## The set of sunctions in this file present the posibility to make inverse
## matrix and cache it value for further usage, becuase inverse matrix
## calculations is CPU intesive process and it is a good idea to reuse already
## calculated inverse matrices 
##
## Example use
## > A<-matrix(c(2,2,3,2),2,2)  initialize matrix
## > B<-makeCacheMatrix(A)      initializing cacheable matrix
## > A%*%cacheSolve(B)          checking the result gives identity matrix
##
## I would like to thank Fu Sheng Wang for the good explanation of the problem
## in the course discussion board. 
## https://class.coursera.org/rprog-002/forum/thread?thread_id=696
##
## Function makeCacheMatrix takes as argument a matrix and and make a special
## "matrix" which by itself is list of function which contain functions
## 1. set matrix value
## 2. get matrix value
## 3. set inverse matrix
## 4. get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  Function cacheSolve returns a matrix which is an inverse matrix of the
## original matrix. if the inverse matrix had already been calculated it returns
## it cached value. If had not been calculated yet, it calculates it, caches it
## and returns it as a result of the function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { ## checks if it is already cached
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ## gets inverse matrix for first time
        x$setinverse(m)       ## cache inverse matrix for first time
        m
}
