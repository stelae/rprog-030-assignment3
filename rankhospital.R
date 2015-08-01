##PART 3 - Ranking hospitals by outcome in a state
#################################################
##Write a function called rankhospital that takes three arguments: the
##two-character abbreviated name of a state (state), an outcome (outcome), and 
##the ranking of a hospital in that state for that outcome (num).
##
##The function reads the outcome-of-care-measures.csv file and returns a 
##character vector with the name of the hospital that has the ranking specified 
##by the num argument. For example, the call 
##rankhospital("MD", "heart failure", 5)
##would return a character vector containing the name of the hospital with the 
##5th lowest 30-day death rate for heart failure. The num argument can take
##values “best”, “worst”, or an integer indicating the ranking (smaller numbers 
##are better). If the number given by num is larger than the number of 
##hospitals in that state, then the function should return NA. Hospitals that 
##do not have data on a particular outcome should be excluded from the set of 
##hospitals when deciding the rankings.
##
##Handling ties.
##It may occur that multiple hospitals have the same 30-day mortality rate 
##for a given cause of death. In those cases ties should be broken by using 
##the hospital name.
##One can use the order function to sort multiple vectors in this manner 
##(i.e. where one vector is used to break ties in another vector).
##
##The function should check the validity of its arguments. If an invalid state 
##value is passed to best, the function should throw an error via the stop 
##function with the exact message “invalid state”. If an invalid outcome value 
##is passed to best, the function should throw an error via the stop function 
##with the exact message “invalid outcome”r.

#SOLUTION
##########################
rankhospital <- function(state, outcome, num = "best") {

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
    #Check if outcome is one of valid
    #"heart attack", "heart failure", or "pneumonia"
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
    
    interestingColIndex <- match(interestingCol, names(careTable))
    for(i in interestingColIndex){
        if (!is.integer(i) && !is.numeric(i)) stop("Unexpected input file.")
    }
    
    #Check if state argument is valid
    #From above, assume that the input table is good, so use it to get
    #a list of valid states.
    stateCodes <- as.factor(careTable[["State"]])
    if( !(state %in% stateCodes)) stop("invalid state")
    
    #Check if num argument is valid
    if( 
        num != "best" && num != "worst" && !is.numeric(num) && !is.integer(num)
    ){
        stop("invalid rank")
    }
    
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