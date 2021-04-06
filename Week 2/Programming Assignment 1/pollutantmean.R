pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        ## Initialize a variable to collect all data points from
        ## selected(id) CSV files, this variable will contain all four
        ## attributes in the selected files
        ## Date, sulfate, nitrate, ID
        collect_all <- NA
        
        ## Adjust the 'specdata' directory
        adjust_directory <- paste(getwd(), "/", directory, "/", sep = "")
        
        ## Extract all 332 CSV files from the adjusted 'specdata' directory
        all_files <- list.files(adjust_directory)
        
        ## Read all id-selected files one by one
        ## Extract all data, and put data to the collect_all variable
        for (x in id) {
                single_file_directory <- paste(adjust_directory, all_files[x], sep = "")
                collect_single <- read.csv(single_file_directory)
                collect_all <- rbind(collect_all, collect_single)
        }
        
        ## Calculate the mean value according to 'pollutant', and
        ## return the mean value result
        mean_answer <- mean(collect_all[[pollutant]], na.rm = TRUE)
        mean_answer
}