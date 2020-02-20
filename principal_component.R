principal_components = function (file_path)
{
  data=read.table(file_path, header=TRUE)
  
  # Center the data, store in matrix X
  col_means=colMeans(data)
  n=nrow(data)
  m=ncol(data)
  X=matrix(0L, nrow=n, ncol=m)

  for (i in 1:m)
    {
        X[,i]=data[,i]-col_means[i] 
    }
  
  #Compute t(X) * X with nondimensional columns
  XTX=(n-1)*cor(X)
  
  #Compute eigenvalues and find principal components 
  eig_XTX=eigen(XTX)
  Z=matrix(0L, nrow=nrow(stock_data), ncol=ncol(stock_data))

  for (i in 1:m)
    {
        Z[,i]= X %*% eig_XTX$vectors[,i]
    }
  
  #Print the proportion of variance explained by each principal component
  total_var=sum(eig_XTX$values)
  print('Proportion of variance explained by each variable is:')
  print(eig_XTX$values/total_var)
  
  #Return matrix Z where the ith column is the ith principal component (i=1,...,m)
  return(Z)
}
