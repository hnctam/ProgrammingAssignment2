## Make the cache matrix, contains list of functions to
##      1. get_matrix() : Gets the input matrix
##      2. get_inverse_matrix() : Gets the inverse matrix
##      3. set_inverse_matrix() : Caches the inverse matrix
##
## @param p_matrix
##              The input matrix
makeCacheMatrix <- function(p_matrix = matrix()) {
    v_inverse_matrix <- NULL

    # Gets the input matrix
    get_matrix <- function() {
        return(p_matrix)
    }

    # Gets the inverse matrix
    get_inverse_matrix <- function() {
        return (v_inverse_matrix)
    }

    # Caches the inverse matrix to avoid duplicated calculation
    set_inverse_matrix <- function(p_inverse_matrix) {
        v_inverse_matrix <<- p_inverse_matrix
    }

    # return a list of functions as an R object
    list(get_matrix=get_matrix,
         get_inverse_matrix=get_inverse_matrix,
         set_inverse_matrix=set_inverse_matrix)
}

## Solves the cached matrix, if found in cache then returns it
##                           if not found in cache then solve the inverse matrix then caches it
## 
## @param p_matrix
##              The input matrix
cacheSolve <- function(p_matrix) {
    ## Retrieves the cached inverse matrix
    v_inverse_matrix <- p_matrix$get_inverse_matrix()
    if (!is.null(v_inverse_matrix)) {
        # Cached matrix found. Retrieves from the cache and returns it...
        message("Found cached instance of inverse matrix, returns it...")
        return (v_inverse_matrix)
    }
    else {
        # Cached matrix is not available
        message("Not found any cached instance of inverse matrix")
        # Retrieves the matrix
        v_matrix <- p_matrix$get_matrix()
        # Solves the inverse matrix
        v_inverse_matrix <- solve(v_matrix)
        # Caches the inverse matrix
        p_matrix$set_inverse_matrix(v_inverse_matrix)
        # Returns the inverse matrix
        return (v_inverse_matrix)
    }
}
