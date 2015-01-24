pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pols()llutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  datavector = numeric()
  for (i in id) {
    full_file_name <-gsub(" ","",paste(directory,"/",sprintf('%03d',i),".CSV"))  
    ##print(full_file_name)
    data_read = read.csv(file=full_file_name, header=TRUE, sep=",")  
    
    datavector = c(datavector, data_read[[pollutant]])
  }
  return(mean(datavector, na.rm = TRUE))
  
}
