## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

##The first fucntion, makeCacheMatrix, takes an argument of a matrix, sets the
## value of the matrix, gets the value of the matrix and sets the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m<<-inverse
        get_inverse <- function() m
        list(set = set, get = get, set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

##cacheSolve checks to see if the inverse of the matrix has already been cached,
##if so, it pulls that cached value, if not, it computes the inverse and then caches
##the computed inverse.

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)){
                message("Getting cached matrix data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$set_inverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
