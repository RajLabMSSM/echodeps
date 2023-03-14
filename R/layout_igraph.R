#' \pkg{igraph} layout
#'
#' Layout function uses any \link[visNetwork]{visIgraphLayout}
#'
#' @inheritParams visNetwork::visIgraphLayout
#' @param ... Additional arguments passed to
#' \link[visNetwork]{visIgraphLayout}.
#' @export
layout_igraph <- function(graph,
                          layout = "layout_nicely",
                          ...){
    visNetwork::visIgraphLayout(
        graph = graph,
        layout = layout,
        randomSeed = 11,
        ...)
}
