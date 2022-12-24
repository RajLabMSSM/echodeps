#' Dependency graph plot
#'
#' Plot a dependency graph between a set of R packages
#' as an interactive network.
#'
#' @param g A graph of class \link[igraph]{igraph}.
#' \emph{Warning:} Large graphs may be slow to interact with.
#' @param hide_tooltip Hide the tooltips by default, and only show them when
#' the cursor is hovering over the respective node.
#' @inheritParams dep_graph
#' @inheritParams visNetwork::visNodes
#' @inheritParams visNetwork::visSave
#' @inheritParams visNetwork::visOptions
#' @export
#' @examples
#' dgc_out <- dep_graph_create(pkg = "rworkflows",
#'                             method = "github")
#' vis <- dep_graph_plot(g = dgc_out$subgraph,
#'                       pkg = dgc_out$pkg)
dep_graph_plot <- function(g,
                           pkg,
                           shape = c("image", "hexagon"),
                           image =
                             file.path(
                               "https://github.com/RajLabMSSM",
                               "Fine_Mapping/blob/master/echolocatoR",
                               "images/bat_silhouette.png?raw=true"),
                           layout = echodeps::layout_star,
                           hide_tooltip = TRUE,
                           show_plot = list(r=TRUE,
                                            browser=TRUE),
                           save_path = file.path(
                               tempdir(), paste0(basename(pkg),
                                                 ".dep_graph.html")),
                           width = NULL,
                           height = NULL,
                           colors = echodeps::construct_colors(),
                           font_face = "Tahoma",
                           verbose = TRUE){
  requireNamespace("visNetwork")
  requireNamespace("igraph")

  #### Check args ####
  if(is.null(layout)) layout <- function(pkg,x) x
  if(is.null(save_path) && isTRUE(show_plot$browser)){
      messager("WARNING: save_path must be a valid file path",
               "to use show_plot with browser=TRUE.",v=verbose)
  }
  shape <- tolower(shape[1])
  #### Make plot ####
  vis <- visNetwork::visIgraph(g) |>
    layout(pkg = pkg) |>
    visNetwork::visNodes(
      shape = shape,
      borderWidth = 2,
      image = image,
      labelHighlightBold = TRUE,
      color = list(
        background = colors$node_background,
        border = colors$node_border,
        highlight = colors$node_highlight,
        hover=list(background=colors$node_hover_background,
                   border=colors$node_hover_border)
      ),
      font = list(color=colors$node_font,
                  size=20,
                  face=font_face,
                  strokeWidth=10,
                  strokeColor=colors$node_font_stroke),
      shadow = list(enabled = TRUE,
                    size = 40,
                    color=colors$node_shadow) # "#03b1f0"
    ) |>
    visNetwork::visEdges(
      arrows = 'from',
      color = list(color = colors$edge_color,
                   opacity=.75,
                   highlight = colors$edge_highlight),
      shadow = list(enabled = TRUE,
                    color = colors$edge_shadow),
      smooth = TRUE,
      dashes =FALSE,
      width = 2
    ) |>
    visNetwork::visOptions(nodesIdSelection = list(enabled = FALSE,
                                                   selected = pkg,
                                                   main = "select package"),
                           highlightNearest = TRUE,
                           width = width,
                           height = height) |>
    visNetwork::visInteraction(
      tooltipStyle =paste(
        paste("position:",if(isTRUE(hide_tooltip))"fixed"else"relative"),
        paste("visibility:",if(isTRUE(hide_tooltip))"hidden"else"visible"),
        paste("font-family:",font_face),
        paste("color:",colors$tooltip_font),
        paste("background-color:",colors$tooltip_background),
        paste("box-shadow: 2px 2px 2px 3px",colors$tooltip_box_shadow),
        "padding: 10px",
        sep=";"))
  #### Save ####
  if(!is.null(save_path)) {
    save_path <- gsub("\n","",save_path)
    dir.create(dirname(save_path),showWarnings = FALSE, recursive = TRUE)
    message("Saving dependency graph plot ==> ",save_path)
    visNetwork::visSave(graph = vis,
                        file = save_path,
                        background = colors$save_background,
                        selfcontained = TRUE)
  }
  #### Show ####
  if(isTRUE(show_plot$r)) {
    messager("Showing plot in R.",v=verbose)
    print(vis)
  }
  if(isTRUE(show_plot$browser) &&
     !is.null(save_path) &&
     file.exists(save_path)){
    messager("Showing plot in browser.",v=verbose)
    utils::browseURL(save_path)
  }
  return(vis)
}
