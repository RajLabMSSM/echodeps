pkg_to_ref <- function(g,
                       pkgs){
    df <- g |> tidygraph::as_tbl_graph() |> data.table::as.data.table()
    df <- echogithub::add_owner_repo(dt = df)
    cols <-  c("package","package","ref","repo","name")
    cols <- cols[cols %in% names(df)][1]
    dict <- c(
        stats::setNames(df$ref,
                        df[[cols]]),
        stats::setNames(df$ref,
                        basename(df[[cols]]))
    )
    if(!is.null(pkgs)){
        return(dict[pkgs])
    } else{
        return(dict)
    }
}
