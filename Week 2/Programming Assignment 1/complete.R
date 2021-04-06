complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1 117
        ## 2 1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

        ## Initialize id and nobs as vectors
        original_id <- vector()
        original_nobs <- vector()
        
        ## Adjust the 'specdata' directory
        adjust_directory <- paste(getwd(), "/", directory, "/", sep = "")
        
        ## Extract all 332 CSV files from the adjusted 'specdata' directory
        all_files <- list.files(adjust_directory)
        
        ## Read all id-selected files one by one
        ## Calculate the sum of number of complete cases
        for (x in id) {
                single_file_directory <- paste(adjust_directory, all_files[x], sep = "")
                collect_single <- read.csv(single_file_directory)
                original_id = c(original_id, x)
                original_nobs = c(original_nobs, sum(complete.cases(collect_single)))
        }
        
        ## Return the data frame according to the requirement
        data_frame_answer <- data.frame(id = original_id, nobs = original_nobs)
        data_frame_answer
}