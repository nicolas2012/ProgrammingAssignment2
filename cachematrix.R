# pair of functions that cache the inverse of a matrix

# this first function creates a list of 4 functions
# set the value of a matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        cm <- NULL
        set <- function(y) {
                x <<- y
                cm <<- NULL
        }
        get <- function() x
        setcachematrix <- function(cachematrix) cm <<- cachematrix
        getcachematrix <- function() cm
        list(set = set, get = get,
             setcachematrix = setcachematrix,
             getcachematrix = getcachematrix)
}


# this function calculate if not already cache the inverse matrix of the matrix x of the above function
cacheSolve <- function(x, ...) {
        cm <- x$getcachematrix()
        if(!is.null(cm)) {
                message("getting cached data")
                return(cm)
        }
        data <- x$get()
        cm <- solve(data, ...)
        x$setcachematrix(cm)
        cm
}
