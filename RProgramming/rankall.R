rankall <- function(outcome, num = "best") {
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
    
    
    short<- data[, c(2,7,col)]
    stated<- split(short, short$State)
    
    for( state in stated){
        inorder<- state[order(state[,2], state[,1], decreasing=decreasing), ]
        if( nrow(inorder) < num ){
            hosp<- "NA"
        }else{
            hosp<- inorder[num,c(1,2)]
        }
        print(hosp)
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}