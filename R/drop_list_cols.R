drop_list_cols <- function(dt){
    dt <- dt[,names(sapply(dt,typeof)!="list"), with=FALSE]
    return(dt)
}
