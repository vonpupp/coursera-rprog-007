best <- function(state, outcome) {
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    states <- unique(data$State)

    state_correct <- toupper(state) %in% states
    if (!state_correct){
        stop("invalid state")
    }
    
    outcome_correct <- tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia")
    if (!outcome_correct){
        stop("invalid outcome")
    }

    by_state <- factor(data$State)
    data_by_state <- split(data, by_state)
    data_by_state$state <- as.numeric(data_by_state$state)
    
    is_heart_attack <- "heart attack" == tolower(outcome)
    is_heart_failure <- "heart failure" == tolower(outcome)
    is_pneumonia <- "pneumonia" == tolower(outcome)
    if (is_heart_attack) {
        outcome_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (is_heart_failure) {
        outcome_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (is_pneumonia) {
        outcome_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    
    data_by_state[[state]][[outcome_column]] <- as.numeric(data_by_state[[state]][[outcome_column]])
    min <- min(data_by_state[[state]][[outcome_column]], na.rm = T)
    
    sorted_data <- data_by_state[[state]][order(data_by_state[[state]][[outcome_column]], data_by_state[[state]][["Hospital.Name"]]),]
    result <- sorted_data[1,][["Hospital.Name"]]
}