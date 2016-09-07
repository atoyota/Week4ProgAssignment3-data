best <- function(state, outcome) {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate

## Read outcome data
rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  # read the Outcome data file
data  <- as.data.frame(cbind(		#constructing necessary data for analysis into a dataframe from rawdata
	rawdata[, 2],   			# extracting hospital column
	rawdata[, 7],   			# extracting state column
	rawdata[, 11],  			# extracting heart attack column
	rawdata[, 17],  			# extracting heart failure column
	rawdata[, 23]), 			# extracting pneumonia column
	stringsAsFactors = FALSE) 	# the character vector not to be converted to a factor
colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") # naming the columns

## Check that state and outcome are valid
    if(!state %in% data[, "state"]){ 	# testing to see if input state name exist in the "state" column
        stop('invalid state')			# stop the function and printing error the message
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){ # testing to see if input given outcome is valid
        stop('invalid outcome: The valid outcome is either heart attack, heart failure, or pneumonia.')
    } else {
        state_answer <- which(data[, "state"] == state)		# set the state to look into
        state_data <- data[state_answer, ]    				# extracting data to analyze for the state
        state_outcome <- as.numeric(state_data[, eval(outcome)], na.rm = TRUE)	# set the outcome data into numeric
        min_val <- min(state_outcome, na.rm = TRUE)			# get min outcome while NA values are ignored.
        result  <- state_data[, "hospital"][which(state_outcome == min_val)] # get the name of hospital with the best outcome
        best_result  <- result[order(result)]				# return the best result
    }
return(best_result)

}