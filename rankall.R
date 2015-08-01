##PART 4 - Ranking hospitals in all states
###########################################

##Write a function called rankall that takes two arguments: an outcome name
##(outcome) and a hospital rank ing (num). The function reads the
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


The function should use the following template.
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}

