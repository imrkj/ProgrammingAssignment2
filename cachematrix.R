## The makeCacheMatrix() and cacheSolve() functions in combination
## compute the inverse of a non-singular and square matrix
## Between these two functions, inverse of a matrix is either retrieved
## from the cache or is computed afresh

## makeCacheMatrix() does following:
# 1. Takes a non-singular, square matrix as argument
# 2. Allows user to input another matrix and its inverse using
# 3. set() & setinverse() functions
# 4. Outputs the set matrix and inverse using get() & getinverse()

# IF LINE 18 IS UNCOMMENTED, THEN cacheSolve() WILL ALWAYS GET THE 
# RESULT FROM CACHE -- WHICH HAS BEEN COMPUTED BY makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
    invofx <- matrix()   # variable assigned for inverse of matrix- x
    
    #invofx <- solve(x)  ## uncommenting this code will always give
                         ## result from cache if matrix is not
                         ##changed using set()
    
    set <- function(y = matrix()){
        x <<- y
        invofx <<- NULL  # if set() changes the matrix
                         # then cacheSolve() computes inverse afresh
    }
    get <- function() {
        x
    }
    setinverse <- function(xinverse = matrix()) {
        
        invofx <<- xinverse # this allows to set the inverse
    }
    getinverse <- function() {
        invofx
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    # output of makeCacheMatrix() is list of funtions
    # assign a variable e.g. matrix1 <- makeCacheMatrix(a-matrix)
}


## cacheSolve() does 4 activities:
# 1. takes an object, such as matrix1 above as an argument
# 2. calls for inverse of matrix from makeCacheMatrix()
# 3. if inverse is already computed, it is returned as output
# 4. if inverse not available, does the computation and sets the 
#    result, i.e. the inverse using setinverse() of makeCacheMatrix()
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    invofx <<- x$getinverse()   # calls for inverse from makeCacheMatrix()
    
    # checks if result is computed already
    if(length(invofx) >1) {
        message("getting cached data")
        return(invofx)
    }
    
    # if result not found, does the computation
    data <- x$get()
    invofx <- solve(data)
    x$setinverse(invofx)  # sets the computed result
    invofx
}

## IF cacheSolve() IS CALLED THE NEXT TIME (i.e. AFTER FIRST TIME)
## IT WILL ALWAYS GET RESULT FROM THE CACHE