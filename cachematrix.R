## Caching the inverse of a Matrix

## This function create a special matrix for caching
makeCacheMatrix <- function(x = matrix()) {      #initialize x as matrix
        inv <- NULL           #initialize inv as NULL
        set <- function(y) {              ##create a set function that takes an argument y
                x<<-y         ##<<- assign y (on the right side of the operator to x (an object in the parent environment)
                inv<<-NULL    ##assign inv to NULL, therefore clears any value of inv that had been cached by a prior excution of makeCacheMaxtrix
        }
        get<-function() x    ##get function call x from parent environment
        setinverse<- function(inverse) inv<<-inverse       ##setinverse is a function of inverse value passed to inv
        getinverse<- function() inv                        ##getinverse retrieve inv value
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    #assign functions as an element within a list() and return to parent environment. Give the name 'set' to set() function defined above...
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## function. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from cache

cacheSolve <- function(x, ...) {        #cacheSolve takes argument x, ... allows to pass any additional arguments into the function
        inv <- x$getinverse()           ##call getinverse()function, apply to x, and pass to inv
        if(!is.null(inv)) {             ##check if inv is NULL, if not
                message("getting cached data")
                return(inv)
        }
           data<-x$get()              ##Call get() that retrieves x and pass to data
           inv<-solve(data,...)       ## Return a matrix that is the inverse of 'x'
           x$setinverse(inv)         ##call setinverse() that passes inv to inv
           inv                       ##return inv
}


###test code
#mat_1 <- matrix(rexp(100),10)
#mat_c <- makeCacheMatrix(mat_1)
#cacheSolve(mat_c)

