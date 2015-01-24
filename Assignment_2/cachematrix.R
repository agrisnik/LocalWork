## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {        
        ##After makeCacheMatrix() function call creates an empty inverted matrix
        inverted_matrix <- NULL
  
        ##sets the matrix
        set <- function(y) {
                x <<- y
                inverted_matrix <<- NULL
        }
        
        ## returns the matrix
        get <- function()  {
                x          
        }
        
        ## sets the inverted matrix
        set_inverse_matrix <- function (inv_matrix) {
                inverted_matrix <<- inv_matrix 
        }
        
        ## returns the inverted matrix
        get_inverse_matrix <- function () {
                inverted_matrix 
        }   
        
        ## makes a member function list
        list(set = set,
             get = get,
             set_inverse_matrix = set_inverse_matrix, 
             get_inverse_matrix = get_inverse_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        result_matrix <- x$get_inverse_matrix()
        if(!is.null(result_matrix)) {
          message("getting cached inverse matrix")
          return(result_matrix)
        }
        
        data <- x$get()
        result_matrix <- solve(data, ...)
        x$set_inverse_matrix(result_matrix)
        
        ## return is not needed, but is put here because I have used to write code in C type languages
        return (result_matrix)
}
