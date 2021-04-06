corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        ## Initialize correlations as a vector
        correlations <- vector()
        
        ## Adjust the 'specdata' directory
        adjust_directory <- paste(getwd(), "/", directory, "/", sep = "")
        
        ## Meet the threshold requirement
        observe <- complete(directory)
        observed = subset(observe, threshold < observe$nobs)
        
        ## Extract all 332 CSV files from the adjusted 'specdata' directory
        all_files <- list.files(adjust_directory)
        
        ## Read all files according to threshold one by one
        ## Calculate the correlation between two vectors
        ## Store the calculated results into the previously defined
        ## vector of correlations
        for (x in observed$id) {
                single_file_directory <- paste(adjust_directory, all_files[x], sep = "")
                collect_single <- read.csv(single_file_directory)
                collect_single <- subset(collect_single, complete.cases(collect_single))
                correlations <- c(correlations, cor(collect_single$sulfate, collect_single$nitrate))
        }
        
        correlations
}