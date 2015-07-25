best <- function(state, outcome){
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        ## Clean parameter
        outcomeVec <- strsplit(outcome, split = " ")[[1]]
        outcomeCap <- paste(toupper(substring(outcomeVec, 1, 1)), 
                           substring(outcomeVec, 2), sep = "", collapse = " ")
        outcomeIn <- gsub(" ", ".", outcomeCap)
        stateIn <- toupper(state)

        ## Check that state and outcome are valid
        if (!any(unique(outcomeData$State==stateIn))){
                stop("invalid state")
        } else if (!any(grep(outcomeIn, colnames(outcomeData), 
                             ignore.case = TRUE))){
                stop("invalid outcome", call. = TRUE)
        }
        
        ## Return hospital name in that state with lowest level 30-day death
        ## rate
        colName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcomeIn, sep = "")
        inState <- outcomeData$State==stateIn
        minValue <- min(suppressWarnings(as.numeric(outcomeData[,colName][inState])), na.rm = TRUE)
        minValueInState <- outcomeData[,colName][inState]==minValue

        hospital<-sort(outcomeData$Hospital.Name[inState][minValueInState], decreasing = FALSE)
        return(hospital[1])
}