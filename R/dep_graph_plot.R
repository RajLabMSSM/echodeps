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
#' @import visNetwork
#' @import igraph
#' @examples
#' dgc_out <- dep_graph_create(pkg = "rworkflows",
#'                             method = "github")
#' vis <- dep_graph_plot(g = dgc_out$subgraph,
#'                       pkg = dgc_out$pkg)
dep_graph_plot <- function(g,
                           pkg,
                           shape = c("image", "hexagon"),
                           image = "hex_sticker",
                           layout = layout_igraph,
                           hide_tooltip = TRUE,
                           show_plot = list(r=TRUE,
                                            browser=TRUE),
                           save_path = file.path(
                               tempdir(), paste0(basename(pkg),
                                                 ".dep_graph.html")),
                           width = NULL,
                           height = NULL,
                           colors = construct_colors(),
                           font_face = "Tahoma",
                           use_basename = FALSE,
                           verbose = TRUE){
    # devoptera::args2vars(dep_graph_plot, reassign = TRUE) 

    #### Check args ####
    if(is.null(layout)) layout <- function(pkg,x) x
    if(is.null(save_path) && isTRUE(show_plot$browser)){
        messager("WARNING: save_path must be a valid file path",
                 "to use show_plot with browser=TRUE.",v=verbose)
    }
    shape <- tolower(shape[1])
    image <- image[1]
    #### Set node names ####
    if(isTRUE(use_basename)){
        igraph::V(g)$name <- basename(igraph::V(g)$name)
        pkg <- basename(pkg)
    } else {
        igraph::V(g)$name <- igraph::V(g)$ref
        pkg <- pkg_to_ref(g = g, pkgs = pkg)
    }
    if(image=="hex_sticker"){
        default_hex <- paste0("https://github.com/RajLabMSSM/echodeps/",
                              "blob/master/inst/hex/hex_default.png?raw=true")
        igraph::V(g)$image <- ifelse(
            is.na(igraph::V(g)$hex_url),
            default_hex,
            paste0(
                gsub("/raw/","/blob/",igraph::V(g)$hex_url),"?raw=true"
            )
        )
        shape <- "image"
    }
    if(image=="bat"){
        image <- file.path(
            "https://github.com/RajLabMSSM",
            "Fine_Mapping/blob/master/echolocatoR",
            "images/bat_silhouette.png?raw=true"
        )
        shape <- "image"
    }

    #### Make plot ####
    vis <-
      visNetwork::toVisNetworkData(g) %>%
      {
        do.call(visNetwork::visNetwork,
                c(., list(main = list(text=NULL,
                                      style="color:white"),
                          submain = list(text=NULL,
                                         style="color:white"),
                          background = colors$save_background,
                          width = width,
                          height = height
                )
                )
        )
      } |>
      # visNetwork::visIgraph(g, type = "full") |>
      layout(pkg) |>
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
                      color=colors$node_shadow) 
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
                             highlightNearest = TRUE) |>
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
