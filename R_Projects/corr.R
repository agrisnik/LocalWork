corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  Correlation_vector = data.frame(Correlations = numeric(0))  
  result_data_frame = data.frame(name = character(0), nobs = numeric(0))
  
  filelist <- list.files(directory)
  
  for(file in filelist)
  {  
    full_file_name <- gsub(" ","",paste(directory,"/",file)) ## sprintf('%03d',i),".CSV")) 
    rawdata <- read.csv(file=full_file_name, header=TRUE, sep=",")
    good <- complete.cases(rawdata) 
    
    if(nrow(rawdata[good,]) > threshold)
    {
      sulfate <- rawdata[good,"sulfate"]
      nitrate <- rawdata[good,"nitrate"]             
      
      
      ##append(Correlation_vector, cor(sulfate,nitrate))
      
      ##print(cor(sulfate,nitrate))
      ##  result_data_frame[nrow(result_data_frame) + 1, ] <- c(i,nrow(rawdata[good,]))
      
      Correlation_vector[nrow(Correlation_vector) + 1,] <- cor(sulfate,nitrate)
      
      ##print(nrow(rawdata[good,]))        
    }
  }
  return(as.vector(Correlation_vector["Correlations"]$Correlations, mode = "numeric"))
  
  ##return (Correlation_vector["Correlations"])
}