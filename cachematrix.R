## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Creating a special object that stores the matrix in a cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #NULL represent a list with zero length, so an empty vector. Once the inverse of the mastrix is solved it will save it in m  
        set <- function(y){
                x <<- y 
                m <<- NULL
        }
        get <- function() x ## get the matrix input
        setinverse <- function(solve) m <<- solve ## solve is the function to get the inverse of the matrix
        getinverse <- function()m # to gettinginversing function
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) { ## x is the list that is given by the makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){ #is.null will return TRUE of FALSE
                message("getting cached data")
                return(m) ## When using return, that's the
                # output that the funcion will get, and therefore, it won't keep running the following lines
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}