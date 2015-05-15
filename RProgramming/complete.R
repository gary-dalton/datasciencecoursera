complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    out<- NULL
    for(value in id){
        fname <- paste(sprintf("%03d", value), "csv", sep=".")
        path<- paste(dir, fname, sep="/")
        data<- read.csv(path)
        u<-complete.cases(data)
        nobs<-length(data$ID[u])
        out<- rbind(out, c(id = value, nobs = nobs))
    }
    out<-as.data.frame(out)
    out
}