showNtell <- function(filename){
  M <- read.table(filename, header=FALSE, sep=" ",quote="")

    # Keep track of fatal problems
    Fatal <- FALSE  
    
    #check numer of columns in input file
    if( length(M[2,]) < 3 )
    {
      Fatal <- TRUE
      print("Checking number of columns in input: Incorrect.")
    }
      else
    {
        print("Checking number of columns in input: Correct.")
    }
    
    #check for zero values in M[,2]
    first = TRUE
    if(M[1,2] == 0 && first == TRUE)
    {
      first = FALSE
      i = 1
      while(M[i+1,2] == 0 && i < length(M[,2]))
      {
        i = i + 1
      }
      num = i
    }
    if(first == FALSE)
    {
      temp_M = M[(num+1):length(data[,1]),]
      len = length(M[,1]) - num
      M = matrix(rep(0,(len)*3),ncol = 3)
      M[1:len,1] = temp_M[,1]
      M[1:len,2] = temp_M[,2]
      M[1:len,3] = temp_M[,3]
      n = len
      out_e = paste("Warning: first ", num, " intensity values equal zero. These values were stripped from the data.",sep="")
      print(out_e)
    }
      else
    {
        print("No zero-value intensities are observed in the input file (this is good).")
    }
     
    newdata=M
   
    #check for negative values in M[,2]
    first = TRUE
    for(i in 1:length(M[,2]))
    {
      if(M[i,2] < 0 && first == TRUE)
      {
        n = i - 1
        first = FALSE
        print("Warning: negative intensity values found.")
        #Make temporary array with shortened data 
        newdata=M[1:n,]
      }
    }
    if(first == TRUE)
    {
      n = length(M[,2])
      print("No negative intensity values found.")
    }
#     
#     #check for correct concavity of intensity curve
#     ##
#     
# #    y3 = diff(diff(diff(log(data[1:n,2]))))
# #    v = cpt.var(y3,know.mean=TRUE,mu=0,test.stat="Normal",method="PELT",penalty="Manual",pen.value=7*log(n))
# #    cp2 = cpts(v)[1]
# #    cp1 = 1
#     
# #    output = fit_cubic_splines(M[1:n,1],M[1:n,2],M[1:n,3],cp1,cp2)
    return(M)
}
