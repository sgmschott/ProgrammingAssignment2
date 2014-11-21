## Source the file cachematrix.r in the console
## create the matrix values to the matrix by m[ , ]<-sample(1:100, 25)
##  This will populate the matrix with the initial values
makeCacheMatrix <- function(x = matrix(),y = list(), z = list()) {
  ## The above line creates the input matrix (X),  the Y matrix is the place where we are going to hang the information ## that is input by the user, the z list is the inverse of the (X) mareix that we will use to capture the 
  ## adjusted information.
  ## This function creates a special "matrix" object that can cache its inverse
  ## x=Input Matrix, y=Matrix Vector Cache to hang the (X), z=Inverse Matrix Vector Cache
  ## Get Input matrix x, See if previously cached by comparing input matrix x with the info cached in y
  ## If y list is empty, we must add the matrix to the cache list, get the inverse, then add inverse to z list, return 
  ## the inverse to the user
  ## Once we get the X matrix, we need to ask Does x match anything in the cache we just created.
  ## When we compare x to Y and in each of the cells we get a true or false.  If they match its TRUE if not its FALSE
  ## We do that with the equation (answer<- x==y[n]; truth<-!answer; sum(truth);if the sum is zero the input matrix was
  ## processed and the answer is now on the z list.  Instead of calculating all, we just check the ones that were not 
  ## matched rather than checking them all.
  ## Loop through every entry in the list to make the same determination to know whether they are the same (a hit)
  ## If no match,add the x matrix to the Y vector cache, obtain inverse matrix, add inverse matrix to z list cache and 
  ## return the Y matrix back to the user
  ## If we get a match, pull answer or the cached inverse matrix from the z list and return the inverse matrix 
  ## If y is the vector cache them y[1]<-list(x) - the 1st cell is inserted into the matrix into position 1 of the vector ## list, keeps hanging the new value into the applicable row  
  nmb_rows<-0
  ##  Need to know how many rows and set it to 0 or setting an initial number to the counter
  nmb_cols<-0
  ## collecting the length of the storage location  
  nmb_matrices_in_ylist<-length(y)
  nmb_matrices_in_zlist<-length(z)
  ## sets us to know where we are in the matrix
  ## No need to check cache on the 1st attempt, get the inverse, cache it and return inverse
  if (length(y)<1)
  {
    Imatrix<-cacheSolve(x,nmb_matrices_in_ylist,y,z)
    print("1st Attempt Check Cached y Matrix")
    print(y)
    thelist<-list(Imatrix,y,z)
    return(thelist)
  } 
  ## The resultant list, the first is a matrix and the second and thord are additional lists.
  ## Loop through all matrices in the cache looking for a match, sum of "truth" will be zero when match is detected
  ## See if the lapply function can be substituted for the "for loop"
  ## Does x = any matrix in the y list (answer<- x==y[n]; truth<-!answer; sum(truth);if the sum is zero we have a match)
  ## If no match,add matrix to vector cache, obtain inverse matrix, add inverse matrix to z list cache and return matrix
  ## If we get a match, pull the cached inverse matrix from the z list and return the inverse matrix 
  ## If y is the vector cache them y[1]<-list(x) would insert the matrix into position 1 of the vector list 
  for (i in 1:nmb_matrices_in_ylist)
  {
    answer<- x==y[[1]] [[i]]
    truth<-!answer
    gotamatch<-sum(truth)
  }
  
  ## When every cell matches the !answer will contain all zeroes and the sum(truth) will equal zero
  print(gotmatch)
  if (gotamatch < 1)
  {
    thelist<-list(Imatrix,y,z)
    return(thelist)
  }  
  
  
  ## No match,so add matrix to vector cache, obtain inverse matrix, add inverse matrix to z list cache and return matrix
  
  Imatrix<-cacheSolve(x,nmb_matricies_in_ylist,y,z)
  thelist<-list(Imatrix,y,z)
  return(thelist)
  
}

## cacheSolve calculates the inverse of the provided matrix and cashes both the original and inverted matrices

cacheSolve <- function(x = matrix, cachepos=integer, y=list(), z=list()) {
  ## cashesolve funtion Return a matrix that is the inverse of 'x'
  print("cacheSolve x Matrix")
  print(x)
  Inverse_Matrix<-solve(x)
  ## cache the inverse of x on the z list at the z length + 1 position and cache the x matrix as well
  position<-cachepos + 1
  z[position]<-list(Inverse_Matrix)
  y[position]<-list(x)
  print("Inverse Matrix")
  print(Inverse_Matrix)
  print("Cached X Matrix")
  print(y[position])
  print("Cached I Matrix")
  print(z[position])
  thelist<-list(Inverse_Matrix,y,z)
  return(thelist)
}

##  The above group returns the original (x) and the cache to compare them
##  To test the code, in the console type the command >retIMatrix
##  This will create the required output