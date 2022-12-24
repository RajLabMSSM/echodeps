add_owner_repo <- function(dt,
                           use_basename,
                           sep){

    owner <- repo <- owner_repo <- subaction <- target <- NULL;
    if(!methods::is(dt,"data.table")) dt <- data.table::data.table(dt)

    #### basename or full name ####
    if(isTRUE(use_basename)){
        dt[,package:=basename(repo)]
    } else {
        #### Add subaction names ####
        if("subaction" %in% names(dt)){
            dt[,owner_repo:=gsub("/",sep,subaction)]
            dt[,package:=gsub("/",sep,subaction)]
        } else {
            dt[,owner_repo:=paste(owner,repo,sep=sep)]
            dt[,package:=owner_repo]
        }
        if("target" %in% names(dt)){
            dt[,target:=gsub("/",sep,target)]
        }
    }
    return(dt)
}
