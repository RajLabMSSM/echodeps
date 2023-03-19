#' \pkg{igraph} layout
#'
#' Layout function uses any \link[visNetwork]{visIgraphLayout}
#'
#' @inheritParams visNetwork::visIgraphLayout
#' @param pkg Unused argument; included here for compatibility
#' with other layout functions.
#' @param ... Additional arguments passed to
#' \link[visNetwork]{visIgraphLayout}.
#' @export
layout_igraph <- function(graph,
                          pkg,
                          layout = "layout_with_kk",
                          ...){
    visNetwork::visIgraphLayout(
        graph = graph,
        layout = layout,
        randomSeed = 11,
        ...)
}
