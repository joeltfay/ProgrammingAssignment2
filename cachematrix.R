## Copied code from example and change word "mean" to "solve", but the
## functions work the same.

## makeCacheMatrix is a list containing four functions: set, get,setSolve and 
## getSolve.

## Example input: objectVar <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol =2))
## assigns the matrix [,1] [,2]
##                [1,]  1    3
##                [2,]  2    4
## into the main function variable x 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               
    set <- function(y) {    ## set changes the matrix stored in
        x <<- y             ## the main function. Example:
        m <<- NULL          ## objectVar$set(matrix(5:8, nrow = 2, ncol = 2))
                            ## replaces the original matrix with: 
                            ##      [,1] [,2]
                            ##  [1,]  5    7
                            ##  [2,]  6    8
}

    get <- function() x ## get returns the matrix stored in the main function
                        ## Example: matrixVar$get()
                        ## returns [,1] [,2]
                        ##     [1,]  5    7
                        ##     [2,]  6    8
    
    setSolve <- function(solve) m <<- solve ## setSolve stores the inverse
    getSolve <- function() m                ## matrix to the variable m 
    list(set = set, get = get,              ## getSolve is available to return 
         setSolve = setSolve,               ## the inverse matrix assigned to m;
         getSolve = getSolve)               ## neither is useful without first
}                                           ## running the cacheSolve function
                                            ## as the variable m will be NULL
                                            ## until then.

## cacheSolve calls the getSolve function to determine whether the variable m
## is NULL. If m is not NULL (!is.null(m)), the function displays a message and
## returns m from cache. If m is NULL, the function calls the solve function to
## invert the matrix and assigns it to m and stores it into cache via setSolve.

cacheSolve <- function(x, ...) {    ## variable same as before (e.g. objectVar)
    m <- x$getSolve()               ## assign getSolve to m (if value in cache)
    if(!is.null(m)) {               ## check whether m is NULL
        message("getting cached data")
        return(m)                   ## if m is not NULL, return a matrix that
                                    ## is the inverse of 'x'
}
    ## if m was NULL, go on to invert the matrix and store in cache                                    
    data <- x$get()                 ## assign matrix to variable data
    m <- solve(data, ...)           ## run function solve to invert matrix and
    x$setSolve(m)                   ## and assign to m. Then run setSolve
    m                               ## function to store inverted matrix in
}                                   ## cache. Return m, the inverted matrix,   
                                    ## to console.

## Example output cacheSolve(objectVar) for initial objectVar matrix above.
##      [,1] [,2]
##  [1,] -2  1.5
##  [2,] 1  -0.5
                                    