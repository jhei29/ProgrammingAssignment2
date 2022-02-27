## This code caches the inverse so continuous computation is not done.
## The functions made creates a matrix and putting the inverse 
## to the cache.

## The function below develops a matrix that iverses the cache.

makeCacheMatrix <- function(df = matrix()) {
    set_inv <- NULL
    set <- function(x) {
    df <<- x
    set_inv <<- NULL
}
    get <- function() df
    Set_Inverse <- function(inverse) set_inv <<- inverse
    Get_Inverse <- function() set_inv
    list(SET = SET, GET = GET, Set_Inverse = Set_Inverse, Get_Inverse = Get_Inverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(df, ...) {
    ## Get the inverse of 'df' and return it
    set_inv <- df$Get_Inverse()
    if (!is.null(set_inv)) {
            return(set_inv)
        }
        
    matr_store <- df$GET()
    set_inv <- solve(matr_store, ...)
    df$Set_Inverse(set_inv)
    set_inv
}
