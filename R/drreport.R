#' Setup an rmarkdown document for Digital Reasoning defaults
#'
#' This function should be called in the first, setup, chunk of the document.
#' 
#' @export
drdocumentsetup <- function() {
  library(knitcitations)
  library(knitr)
  library(xtable)
  library(ggplot2)
  library(pander)
  library(magrittr)
  library(tidyr)
  library(dplyr)
  
  opts_knit$set(
    echo = FALSE,
    tidy = TRUE,
    cache = FALSE
  )
  
  .drformat <<- opts_knit$get("rmarkdown.pandoc.to")
  
  if (.drformat == "html") {
    library(plotly)
    opts_knit$set(
      fig.margin=TRUE,
      dev="svg"
    )  
  } else {
    opts_knit$set(
      fig.margin=TRUE,
      dev="tikz"
    )
  }
  
  drplot <<- function(...) {
    plt <- eval(enquote(...))
    if (.drformat == "html") ggplotly(plt)
    else plt
  }
  
  cite_options(
    citation_format = "pandoc",
    style="citation"
  )
  
  options(xtable.comment = FALSE)
  options(xtable.booktabs = TRUE)
  
  theme_set(
    theme_bw() %+replace%
      theme(
        legend.key.size = unit(4, "mm"), 
        legend.title = element_text(size = rel(0.8),
                                    face = "bold"),
        legend.margin = unit(0, "cm"),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "lines"),
        legend.text=element_text(size = unit(8, "points")), 
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(size = rel(0.7)),
        plot.margin = unit(c(0, 0.5, 1, 0), "lines"), 
        axis.title = element_text(size = rel(0.8),
                                  face = "bold"),
        title = element_text(size = rel(0.9))
      ) 
  )
  
  drblue <<- "#006699"
  drbrown <<- "#CC9934"
  drgrey <<- "#7A7A7A"
  drgreen <<- "#669933"
  drpallette <<- c(drblue, drbrown, drgrey, drgreen)
  
  update_geom_defaults("point", list(colour = drbrown))
  update_geom_defaults("line", list(colour = drbrown))
  
  colors_divergent_discrete <<- function(x) 
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(x, "Spectral"))
}