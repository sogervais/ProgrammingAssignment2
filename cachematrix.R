# Stephen Gervais
# 2015-01-22
#
# R Programming Course
# Programming Assignment 2

# Assignment 
# ==========
# Write an R function that is able to cache potentially time-consuming 
# computations. For example, taking the mean of a numeric vector is typically a 
# fast operation. However, for a very long vector, it may take too long to 
# compute the mean, especially if it has to be computed repeatedly (e.g. in a 
# loop). If the contents of a vector are not changing, it may make sense to 
# cache the value of the mean so that when we need it again, it can be looked up
# in the cache rather than recomputed. In this Programming Assignment you will 
# take advantage of the scoping rules of the R language and how they can be 
# manipulated to preserve state inside of an R object.

# Function Overview
# =================
#   makeCacheMatrix
#   This function creates a special "matrix" object that can cache its inverse.
#   It consists of 4 functions:
#     setMatrix which sets the matrix
#     getMatrix which gets the matrix
#     setMatrixInverse which caches the matrix inverse
#     getMatrixInverse which returns the cahched value of the matrix solution
# 
#   cacheSolve*
#   This function solves the matrix and caches the resultant inverse
#     * As written, this function requires input to be a square matrix


# Declaring a function makeCacheMatrix that acts on some matrix x and
# caches the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # cache for matrix assignment needed.  
  # create this and initialize with NULL
  cacheMatrix <- NULL
  
  # set the matrix.   
  setMatrix <- function(y) {
    
    # x is assigned "new" matrix argument y 
    x <<- y
    
    # since the matrix is new, reset the cache
    cacheMatrix <<- NULL
  }
  
  # get the matrix
  getMatrix <- function() {
    
    # return x
    x
  }
  
  
  # cache the solved matrix
  setMatrixInverse <- function(solvedMatrix) {
    
    # write the matrix solution to the cache
    cacheMatrix <<-solvedMatrix
    
  }
  
  # return the matrix solution from the cache
  getMatrixInverse <- function() {
    
    # get the solution from the cache
    cacheMatrix
      
  }
  
  # return a list where each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)

}



# Return a matrix that is the inverse of matrix 'x' created and cached with the
# makeCacheMatrix function
cacheSolve <- function(x, ...) {
  
  # get the cached value of the matrix solution
  solvedMatrix <- x$getMatrixInverse()
  
  # if a cached value exists return it
  if(!is.null(solvedMatrix)) {
    message("getting cached data")
    return(solvedMatrix)
  }
  
  else{
  # if a cached value does not exist, get the matrix, caclulate the inverse 
  # and store it in the cache
  matrix <- x$getMatrix()
  solvedMatrix <- solve(matrix)
  x$setMatrixInverse(solvedMatrix)
  
  # return the inverse
  solvedMatrix
  }
}


# # Test the functions.  Should store matrix, calc inverse and cache result
# 
# # Create a 3x3 matrix
# x <- makeCacheMatrix(matrix(cbind(1,2,3,2,3,1,3,2,1), nrow = 3, ncol = 3))
# 
# # Return the function list
# summary(x)
# 
# # Return the matrix
# x$getMatrix()
# 
# # Let's test the caching function
# 
# # the 1st time we run the function, we get the computed value
# tmStart1 <- proc.time()
# cacheSolve(x)
# 
# # Store the time to process
# tmComplete1 <-proc.time() - tmStart1
# 
# # the 2nd time we run the function,we get the cached value
# tmStart2 <- proc.time()
# cacheSolve(x)
# 
# tmComplete2 <-proc.time() - tmStart2
# 
# # Verify, if possible, that first timing is longer than second
# tmComplete1
# tmComplete2