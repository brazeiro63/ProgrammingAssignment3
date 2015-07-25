setwd("D:/Users/Brazeiro/GitRepos/ProgrammingAssignment3")
outcome <- "heart attack"
num <- "worst" 
## Read outcome data
outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcomeVec <- strsplit(outcome, split = " ")[[1]]
outcomeCap <- paste(toupper(substring(outcomeVec, 1, 1)), substring(outcomeVec, 2), sep = "", collapse = " ")
outcomeIn <- gsub(" ", ".", outcomeCap)
if (!any(grep(outcomeIn, colnames(outcomeData), ignore.case = TRUE))){stop("invalid outcome", call. = TRUE)}
colName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcomeIn, sep = "")
hospitalList <- outcomeData[inState,c("Hospital.Name", "State", colName)]
hospitalList[,colName] <- suppressWarnings(as.numeric(hospitalList[,colName]))
hospitalClean <- hospitalList[! is.na(hospitalList[,colName]),]
ranking = hospitalClean[order(hospitalClean[,2], hospitalClean[,1], decreasing = c(FALSE, FALSE)),]
if (num=="best"){ind <- 1}else if (num=="worst"){ind <- nrow(ranking)}else{ind <- num}
