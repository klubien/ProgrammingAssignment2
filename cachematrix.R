## Function that caches the inverse of matrix

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(my_matrix = matrix()) {
        inversed_matrix  <- NULL
        set  <- function(pass_matrix) {
                my_matrix  <<- pass_matrix
                inversed_matrix <<- NULL 
        }
        get  <- function() {my_matrix}
        
        set_inversed_matrix  <- function(pass_inversed_matrix) 
                {inversed_matrix <<- pass_inversed_matrix}
        get_inversed_matrix  <- function() {inversed_matrix}
        
        list(set = set, get = get, 
             set_inversed_matrix = set_inversed_matrix,
             get_inversed_matrix = get_inversed_matrix)

}



## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed_matrix  <- x$get_inversed_matrix()
        if(!is.null(inversed_matrix)) {
                message("getting cached data")
                return(inversed_matrix) 
        }
        my_matrix  <- x$get()
        inversed_matrix  <- solve(my_matrix, ...)
        x$set_inversed_matrix(inversed_matrix)
        inversed_matrix
        
}
