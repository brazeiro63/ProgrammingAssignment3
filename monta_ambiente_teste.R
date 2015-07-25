setwd("D:/Users/Brazeiro/GitRepos/ProgrammingAssignment3")
state <- "md"
outcome <- "heart attack"
num <- "worst" 
outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcomeVec <- strsplit(outcome, split = " ")[[1]]
outcomeCap <- paste(toupper(substring(outcomeVec, 1, 1)),
substring(outcomeVec, 2), sep = "", collapse = " ")
outcomeIn <- gsub(" ", ".", outcomeCap)
stateIn <- toupper(state)
if (!any(unique(outcomeData$State==stateIn))){stop("invalid state")} else if (!any(grep(outcomeIn, colnames(outcomeData),ignore.case = TRUE))){stop("invalid outcome", call. = TRUE)}
colName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcomeIn, sep = "")
inState <- outcomeData$State==stateIn
hospitalList <- outcomeData[inState,c("Hospital.Name", colName)]
hospitalList[,colName] <- suppressWarnings(as.numeric(hospitalList[,colName]))
hospitalClean <- hospitalList[! is.na(hospitalList[,colName]),]
ranking = hospitalClean[order(hospitalClean[,2], hospitalClean[,1],decreasing = c(FALSE, FALSE)),]
if (num=="best"){ind <- 1}else if (num=="worst"){ind <- nrow(ranking)}else{ind <- num}

