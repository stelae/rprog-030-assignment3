##PART 4 - Ranking hospitals in all states
###########################################

##Write a function called rankall that takes two arguments: an outcome name
##(outcome) and a hospital ranking (num). The function reads the
##outcome-of-care-measures.csv file and returns a 2-column data frame containing
##the hospital in each state that has the ranking specified in num. For example
##the function call rankall("heart attack", "best") would return a data frame
##containing the names of the hospitals that are the best in their respective
##states for 30-day heart attack death rates. The function should return a value
##for every state (some may be NA). The first column in the data frame is named
##hospital, which contains the hospital name, and the second column is named
##state, which contains the 2-character abbreviation for the state name.
##Hospitals that do not have data on a particular outcome should be excluded
##from the set of hospitals when deciding the rankings.
##
##Handling ties
##The rankall function should handle ties in the 30-day mortality rates in the
##same way that the rankhospital function handles ties (by hospital name)..
##
##The function should check the validity of its arguments. If an invalid outcome
##value is passed to rankall, the function should throw an error via the stop
##function with the exact message “invalid outcome”. The num variable can take
##values “best”, “worst”, or an integer indicating the ranking (smaller numbers
##are better). If the number given by num is larger than the number of hospitals
##in that state, then the function should return NA.


#SOLUTION
##########

rankall <- function(outcome, num = "best") {
#Reads the standard "outcome-of-care-measures.csv" from the working directory
#and, for a given outcome and a ranking num, it returns a data frame with
#columns "hospital" and "state", showing respectively the hospital from each
#state that was ranked at num for the 30-day mortality rate of the given outcome
#and the two-letter code of the hospital's state.
    
    ######################################
    #Check if file exists and import data.
    ######################################
    filename <- "outcome-of-care-measures.csv"
    if( filename %in% dir() ) {
        careTable <- read.csv(filename, colClasses = "character")
    }else{
        stop("Could not find data in working directory.")
    }
    
    ############################################
    #Check validity of input file and arguments
    ############################################
    #Check if outcome is one of valid "heart attack", "heart failure", or
    #"pneumonia"
    possOutcome <- c("heart attack", "heart failure", "pneumonia")
    if( !(outcome %in% possOutcome) ){
        stop("invalid outcome")
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
    
    interestingColIndex <<- match(interestingCol, names(careTable))
    for(i in interestingColIndex){
        if (!is.integer(i) && !is.numeric(i)) stop("Unexpected input file.")
    }
    
    #Check if num argument is valid
    if( 
        num != "best" && num != "worst" && !is.numeric(num) && !is.integer(num)
    ){
        stop("invalid rank")
    }
    
    ####################
    #Get table of results
    ####################
    #For each state, find name of hospital with given ranking and return list
    #in a data frame with columns "hospital" (for the name) and "state" (for the
    #two-letter codes of states).
    
    #Get list of states
    stateCodes <- levels(as.factor(careTable[["State"]]))
    
    #Initialise result data.frame
    result <- data.frame("hospital" = character(0), "state" = character(0),
                         stringsAsFactors=FALSE)

    #Use rankHospital function (defined below) to get hospital for each state
    #and add to data frame. rankHospital returns a string or NA.
    for(currState in stateCodes){
        result[nrow(result)+1,2] <- currState
        result[nrow(result),1] <- rankHospital(currState, outcome, num, 
                                               careTable)
    }
    
    return(result)

}
    
rankHospital <- function(state, outcome, num = "best", careTable = careTable) {
#This function is meant to be called from the rankall function, therefore all
#the arguments passed are checked by rankall and are assumed to be valid here.
#The function looks within the careTable and, for the given state, returns a 
#string of the hospital name that had rank = num for the given outcome.
#Returns NA if out-of-bounds ranking is used.

    ######################################
    #Partition for given state and outcome
    ######################################
    #Filter for given state
    stateFiltered <- careTable[careTable$State == state, interestingColIndex]
    colClasses <- c("character", "character", "numeric", "numeric", "numeric")
    for(i in 1:length(colClasses)) {
        suppressWarnings(
            class(stateFiltered[[i]]) <- colClasses[i]
        )
    }
    
    #Select column for given outcome
    heartAttackInd <- 3
    heartFailInd <- 4
    pneumoniaInd <- 5
    
    if(outcome == "heart attack"){
        outcomeInd <- heartAttackInd
    } else if( outcome == "heart failure"){
        outcomeInd <- heartFailInd
    } else if( outcome == "pneumonia"){
        outcomeInd <- pneumoniaInd
    } else {
        stop("Something unexpected happened.")
    }
    
    #Get ranked list by mortality rate of given outcome and for given state
    #
    #Keep outcome of interest
    filteredHospitals <- stateFiltered[,c(2,outcomeInd)]
    #Remove NA values
    cleanedHospitals <- 
        filteredHospitals[!is.na(filteredHospitals[[2]]), ]
    #Order list
    permut <- order(cleanedHospitals[2],cleanedHospitals[1])
    rankedHospitals <- cleanedHospitals[permut,]
    
    ########################
    #Return requested result
    ########################
    maxRank <- nrow(rankedHospitals)  #max rank in current list
    
    #Convert words to appropriate rank numbers.
    if(num == "best"){
        num <- 1L
    } else if ( num == "worst") {
        num <- maxRank
    }
    
    #Reject non-integer values
    if( num != as.integer(num) ) {
        stop("Only integer values are valid ranks.")
    }
    
    #Return hospital name if rank within current limits; return "NA" 
    #for anything else.
    if( num >= 1 && num <= maxRank){
        return(rankedHospitals[[num,1]])
    } else {
        return(NA)
    }
    
}