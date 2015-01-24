complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  result_data_frame = data.frame(id = numeric(0), nobs = numeric(0))
  
  for(i in id)
  {  
        full_file_name <- gsub(" ","",paste(directory,"/",sprintf('%03d',i),".CSV")) 
        rawdata <- read.csv(file=full_file_name, header=TRUE, sep=",")
        good <- complete.cases(rawdata) 
        result_data_frame[nrow(result_data_frame) + 1, ] <- c(i,nrow(rawdata[good,]))
  }  
  return(result_data_frame)
}