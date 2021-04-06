## Predefine the validity of 'outcome'
outcome_check <- c("heart failure", "pneumonia", "heart attack")

rankall <- function(outcome, num = "best") {
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
	    outcome_data <- outcome_data[ ,c("hospital.name", "state", outcome_tie)]
	    names(outcome_data) <- c("hospital.name", "state", "outcome")

	    ## Initialize variables for the looping
	    state_vector <- character(0)
	    outcome_vector <- double(0)
	    hospital_vector <- character(0)

	    ## For each state, find the hospital of the given rank
	    for (each_state in unique(outcome_data$state)) {
	    	    each_state_data <- outcome_data[outcome_data$state == each_state & outcome_data$outcome != "Not Available", ]
	    	    each_state_data$outcome <- as.double(each_state_data$outcome)
	    	    each_state_data <- each_state_data[order(each_state_data$outcome, each_state_data$hospital.name), ]

	    	    ## Store the appropriate hospital name and state name into 'hospital_vector' and 'state_vector' for each loop
	    	    ## according to the input variable 'num'
	    	    if (num == "best") {
	    	    	    each_hospital <- each_state_data[1,"hospital.name"]
	    	    }
	    	    else if (num == "worst") {
	    	    	    each_hospital <- each_state_data[nrow(each_state_data),"hospital.name"]
	    	    }
	    	    else {
	    	    	    each_hospital <- each_state_data[num,"hospital.name"]
	    	    }
	    	    hospital_vector <- c(hospital_vector, each_hospital)
	    	    state_vector <- c(state_vector, each_state)
	    }

	    ## Rank all
	    ## Find a data frame with the hospital names and the the (abbreviated) state name
	    answer <- data.frame(hospital_vector, state_vector)
	    answer <- answer[order(answer$state), ]
	    names(answer) <- c("hospital", "state")

	    ## Return a data frame with the hospital names and the (abbreviated) state name
	    answer
	    
}