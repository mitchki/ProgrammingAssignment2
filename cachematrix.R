## makeCacheMatrix creates a list of functions that set and get and cache the inverse
## of a supplied matrix
## 
## cacheSolve gets the inverse of the matrix 
##  from the makeCacheMatrix environment 
##  if it exists, and otherwise inverts the matrix
## and displays it


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


## This function grabs the solved matrix if it exists
## Otherwise it calculates and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

### to test, after defining the above 2 functions, follow below

## run makeCacheMatrix on any invertable test matrix & assign to another object 
 mattest <- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0),3,3,TRUE))

## verify that no inverse matrix is currently stored (returns NULL)
mattest$getinverse()

## run chacheSolve on the test object to create 
## and return the matrix inverse
cacheSolve(mattest)

## verify that the matrix inverse is now cached
mattest$getinverse()

## verify that cacheSolve now returns cached data
cacheSolve(mattest)



