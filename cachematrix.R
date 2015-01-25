## The following two functions helps us caching invrse of a matrix
##(given the matrix is invertible) so as to reduce time consuming 
##computations

##The first function, makeMatrix creates a special "Matrix", 
##which is really a list containing a function to
##1.Get the value of the Matrix
##2.Get the value of the Matrix
##3.Set the value of the Inverse Matrix
##4.Get the value of the Inverse Matrix

makeCacheMatrix = function(x = matrix()) {
    inmat = NULL
    set <- function(y) {
        x <<- y
        inmat <<- NULL
    }
    get = function(){ 
    x  }
    setinverse = function(inverse){
    inmat <<- inverse  }
    getinverse = function(){ 
    inmat  }    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##The following function returns the inverse of the special matrix created in the first function.
##It takes the list returned from the first function as its argument.
##It first checks if the inverse has already been computed.
##If so,it skips the computational part and directly returns the inverse and gives "getting cached data".
##If not,it computes the inverse of the matrix and sets the value in the cache function via setInverse function.

cacheSolve = function(x, ...) {
    inmat = x$getinverse()
    if(!is.null(inmat)) {
        message("getting cached data.")
        return(inmat)
    }
    originalmatrix = x$get()
    inmat = solve(originalmatrix)
    x$setinverse(inmat)
    inmat
}

##Running the program
##Sample1
##> x=matrix(1:4,2,2)
##> m=makeCacheMatrix(x)

##getting the matrix

##> m$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##Since no cache in the first run,so second function will compute the
##inverse
##> cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##Retrieve cache in the second run
##> cacheSolve(m)
##getting cached data.
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##Sample 2
##> x=diag(c(2,2,2),3,3)
##> m=makeCacheMatrix(x)

##getting the matrix
##> m$get()
##     [,1] [,2] [,3]
##[1,]    2    0    0
##[2,]    0    2    0
##[3,]    0    0    2
 
##No cache in the first run
##> cacheSolve(m)
##     [,1] [,2] [,3]
##[1,]  0.5  0.0  0.0
##[2,]  0.0  0.5  0.0
##[3,]  0.0  0.0  0.5

##Retrieve cache in second run
##> cacheSolve(m)
##getting cached data.
##     [,1] [,2] [,3]
##[1,]  0.5  0.0  0.0
##[2,]  0.0  0.5  0.0
##[3,]  0.0  0.0  0.5
