# Fonction pour la preparation des donnees
Stacked_Matrix <- function(matrix_NonStacked){ 
  Matrix_Stacked <- matrix(nrow = (nrow(matrix_NonStacked)) * (ncol(matrix_NonStacked)-2), ncol = 4)
  colnames(Matrix_Stacked) <- c("IdFeature", "Lipides", "IdSample", "Intensity")
  
  Matrix_Stacked[1:nrow(matrix_NonStacked),1] <- as.matrix(matrix_NonStacked[,1])
  Matrix_Stacked[1:nrow(matrix_NonStacked),2] <- as.matrix(matrix_NonStacked[,2])
  Matrix_Stacked[1:nrow(matrix_NonStacked),3] <- colnames(as.matrix(matrix_NonStacked[3]))
  Matrix_Stacked[1:nrow(matrix_NonStacked),4] <- as.matrix(matrix_NonStacked[,4])
  
  row = nrow(matrix_NonStacked)
  j = 0
  for (i in 4:ncol(matrix_NonStacked))
  {
    j = length(which(!is.na(Matrix_Stacked[,1])))
    
    Matrix_Stacked[(j + 1):(j + row),4] <- as.matrix(matrix_NonStacked[,i])
    Matrix_Stacked[(j + 1):(j + row),2] <- as.matrix(matrix_NonStacked[,i])
    Matrix_Stacked[(j + 1):(j + row),3] <- colnames(matrix_NonStacked[i])
    Matrix_Stacked[(j + 1):(j + row),1] <- as.matrix(matrix_NonStacked[,1])
    print(i)
  }
  
  return(as.data.frame(Matrix_Stacked))
}
