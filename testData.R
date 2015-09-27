

print("Matrix 1")
testmatrix1 <- matrix( c(1,-1,1,1), nrow = 2)
func <- makeCacheMatrix (testmatrix3) 
print ("Input Matrix")
print (testmatrix1)
result <-cacheSolve(func)
print ("Inverse")
print (result)
result <-cacheSolve(func)
print ("Inverse from Cache")
print (result)
print ("product of matrix and inverse (identity matrix)")
testmatrix1  %*%  result

print ("")
print ("")

message("Matrix 2")
testmatrix2 <- rbind(c(2, 6, 2), c(1, 4, 2), c (5, 9 ,0))
func <- makeCacheMatrix (testmatrix4) 
print ("Input Matrix")
print (testmatrix2)
result <-cacheSolve(func)
print ("Inverse")
print (result)
result <-cacheSolve(func)
print ("Inverse from Cache")
print (result)
print ("product of matrix and inverse (identity matrix)")
testmatrix2  %*%  result