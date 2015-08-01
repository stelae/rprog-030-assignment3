##PART 2 - Finding the best hospital in a state
######################################################
##Write a function called best that takes two arguments: the 2-character
##abbreviated name of a state and an outcome name.  The function reads the
##outcome-of-care-measures.csv file and returns a character vector with  the  
##name  of  the  hospital  that  has  the  best  (i.e.   lowest)  30-day 
##mortality  for  the  specified  outcome in that state.  The hospital name is 
##the name provided in the Hospital.Name variable.  The outcomes can be one of 
##"heart attack", "heart failure", or "pneumonia".  Hospitals that do not have 
##data on a particular outcome should be excluded from the set of hospitals
##when deciding the rankings.

##Handling ties
##If there is a tie for the best hospital for a given outcome, then the 
##hospital names should be sorted in alphabetical order and the first hospital 
##in that set should be chosen (i.e.  if hospitals "b", "c", and "f" are tied 
##for best, then hospital "b" should be returned).

##The function should use the following template.
#best <- function(state, outcome) {
# Read outcome data
# Check that state and outcome are valid
# Return hospital name in that state with lowest 30-day death
# rate
#}

##The function should check the validity of its arguments.  If an invalid
##state value is passed to best, the function should throw an error via the
##stop function with the exact message "invalid state".

#SOLUTION
#Read in outcomes table, partition for given state and outcome and look for 
#min.

best <- function(state, outcome){
    #Import data
    careTable <- read.csv("outcome-of-care-measures.csv")
    
    #Check validity of input file and arguments
    ############################################
    #Check if outcome is one of valid
    #"heart attack", "heart failure", or "pneumonia"
    possOutcome <- c("heart attack", "heart failure", "pneumonia")
    if( !(outcome %in% possOutcome) ){
        stop("Second argument ('outcome') must
            be one of 'heart attack', 'heart failure', or 'pneumonia'.")
    }
    
    
    #Check if input file has correct column #
    if( !is.data.frame(careTable) ||  ncol(careTable) != 46 ){
        stop("Unexpected data file.")
    }
    
    #Check if input file contains columns of interest
    interestingCol <- c("State", "Hospital.Name",
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    
    interestingColIndex <- match(interestingCol, names(careTable))
    for(i in interestingColIndex){
        if (!is.integer(i) && !is.numeric(i)) stop("Unexpected input file.")
    }

    #Check if state argument is valid
    #From above, assume that the input table is good, so use it to get
    #a list of valid states.
    stateCodes <- as.factor(careTable[["State"]])
    if( !(state %in% stateCodes)) stop("invalid state")
    
    #Partition for given state
    ############################
    interestingData <- careTable[careTable$State == state, interestingColIndex]
    
    
    
}

best("AL","heart attack")
