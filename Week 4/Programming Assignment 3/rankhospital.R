## Predefine the validity of 'outcome'
outcome_check <- c("heart failure", "pneumonia", "heart attack")


rankhospital <- function(state, outcome, num = "best") {
	    ## Check the validity of 'state'
	    if ((state %in% state.abb) == FALSE) {
	    	    stop("invalid state")
	    }
	    ## Check the validity of 'outcome'
	    if ((outcome %in% outcome_check) == FALSE) {
	    	    stop("invalid outcome")
	    }

	    ## Read outcome data
	    outcome_data <- read.csv(file = "outcome-of-care-measures.csv", colClasses = "character")

	    ## Deal with each column using names() function
	    names(outcome_data) <- gsub("\\.\\.\\.", ".", names(outcome_data))
	    names(outcome_data) <- gsub("\\.\\.", ".", names(outcome_data))
	    names(outcome_data) <- tolower(names(outcome_data))

	    ## Handling ties
	    outcome_tie <- gsub(" ", ".", outcome)
	    outcome_tie <- paste("hospital.30.day.death.mortality.rates.from", outcome_tie, sep = ".")
	    outcome_data <- outcome_data[outcome_data$state == state & outcome_data[outcome_tie] != "Not Available", ]
	    outcome_data[ ,outcome_tie] <- as.double(outcome_data[ ,outcome_tie])

	    ## Find hospital name in that state with the given rank 30-day death rate
	    outcome_data <- outcome_data[order(outcome_data[ ,outcome_tie], outcome_data$hospital.name), ]
	    if (num == "best") {
	    	    answer <- outcome_data[1,"hospital.name"]
	    }
	    else if (num == "worst") {
	    	    answer <- outcome_data[nrow(outcome_data),"hospital.name"]
	    }
	    else {
	    	    answer <- outcome_data[num,"hospital.name"]
	    }

	    ## Return hospital name in that state with the given rank 30-day death rate
	    answer
	    
}