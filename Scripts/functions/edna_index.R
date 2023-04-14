#eDNA Index 
#goal: this function calculates the eDNA index on a taxonomy by sample matrix from raw read numbers
#Author: Anya Mueller
#adapted from (Jacobs-Palmer et al, 2020)

edna_index <- function(df, row_var) {
  #df = a dataframe with taxonomy/ASV in columns, samples in rows
  #row_var = variable from the dataframe of desired row-wise identifier
  
  ##prep data
  #make dataframe
  x <- as.data.frame(df)
  #put row_var into rownames
  rownames(x) <- x[[row_var]]
  #remove row_var column
  x[[row_var]] <- NULL
  #remove rows who sum to zero
  x <- subset(x, rowSums(x)>0) 
  
  ##Take proportions from table of raw read counts 
  #apply operation to a matrix
  x <- sweep(x, 
             #do it by row
             MARGIN = 1,
             #value used for operation (sum by sample)
             STATS = rowSums(x), 
             #divide raw read number by row sum
             FUN = "/")
  
  #remove columns who sum to zero
  x[,-(which(colSums(x)==0))] 
  
  ##Take indices using a table with proportions of reads 
  #apply operation to a matrix
  x <- sweep(x, 
             #do it by column
             MARGIN = 2, 
             #value used for operation (row max)
             STATS =
               apply(x, 
                     #do it by column
                     MARGIN = 2, 
                     #take max of column (max proportion for taxonomy/ASV)
                     max), 
             #divide proportion by column max
             FUN = "/")
  
  ##add back row variable
  x[[row_var]] <- rownames(x)
  
  return(x)
}
