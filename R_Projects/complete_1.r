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
  
  if(length(id) == 1)
  {
        full_file_name <- gsub(" ","",paste(directory,"/",sprintf('%03d',id),".CSV"))      
        number_of_nobs <- read_complete_records(full_file_name)
        result_data_frame[1,] <- c(id,number_of_nobs)
  }
  else
  {
        for(i in id)
        {
            full_file_name <- gsub(" ","",paste(directory,"/",sprintf('%03d',i),".CSV"))   
            number_of_nobs <- read_complete_records(full_file_name)
            result_data_frame[nrow(result_data_frame) + 1, ] <- c(i,number_of_nobs)
        }
  }
  return(result_data_frame)
}

read_complete_records <- function(fullfilename) {
  rawdata <- read.csv(file=fullfilename, header=TRUE, sep=",")
  good <- complete.cases(rawdata)
  return(nrow(rawdata[good,]))
}

