best <- function(state, outcome) {
    
    ## Check that outcome is valid
    if(outcome == "heart attack"){
        col<- 11
    }else if(outcome == "heart failure"){
        col<- 17
    }else if(outcome == "pneumonia"){
        col<- 23
    }else{
        stop("invalid outcome") 
    }
    
    ## Read outcome data
    path<- "C:/Users/Gary/Documents/OnlineCourses/DataScience/RProgramming/Week04"
    fname<- "outcome-of-care-measures.csv"
    data<- read.csv(file.path(path, fname), colClasses = "character")
    ## Set fields as numeric
    data[, col] <- suppressWarnings(as.numeric(data[, col]))
    
    ## Get and validate outcomes
    forstate<- data[data$State == state, c(2,col)]
    if( nrow(forstate) < 1 ){
        stop("invalid state")
    }
    
    ## Sort outcomes and return the best
    inorder<- forstate[order(forstate[,2], forstate[,1]), ]
    inorder[1,1]

    
    ## Check that state and outcome are validbest("MD", "heart attack")
    ## Return hospital name in that state with lowest 30-day death
    ## rate

    ## col 11: "heart attack" 30-day mortality
    ## col 17: "heart failure"
    ## col 23: "pneumonia"
}


rankhospital <- function(state, outcome, num="best"){
    ## Check that outcome is valid
    if(outcome == "heart attack"){
        col<- 11
    }else if(outcome == "heart failure"){
        col<- 17
    }else if(outcome == "pneumonia"){
        col<- 23
    }else{
        stop("invalid outcome") 
    }
    
    ## Check that num is valid
    decreasing<- FALSE
    if(num == "best"){
        num<- 1
    }else if(num == "worst"){
        num<- 1
        decreasing<- TRUE
    }else if(is.numeric(num)){
        ## pass
    }else{
        stop("invald num") 
    }
    
    ## Read outcome data
    path<- "C:/Users/Gary/Documents/OnlineCourses/DataScience/RProgramming/Week04"
    fname<- "outcome-of-care-measures.csv"
    data<- read.csv(file.path(path, fname), colClasses = "character")
    ## Set fields as numeric
    data[, col] <- suppressWarnings(as.numeric(data[, col]))
    
    ## Get and validate state
    forstate<- data[data$State == state, c(2,col)]
    if( nrow(forstate) < 1 ){
        stop("invalid state")
    }
    forstate<- forstate[!is.na(forstate[,2]), ]
    
    ## Sort outcomes and return the best
    inorder<- forstate[order(forstate[,2], forstate[,1], decreasing=decreasing), ]
    
    ## Return hospital name in that state with the given rank
    if( nrow(inorder) < num ){
        hosp<- "NA"
    }else{
        hosp<- inorder[num,1]
    }
    hosp
    
}
