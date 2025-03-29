#' w2ui tabs
#'
#' @param inputId The input slot that will be used to access the value
#' @param tabs Initial tab names (character vector)
#' @param width Width of the widget
#' @param height Height of the widget
#' @export
tabs <- function(inputId, tabs = "script.R", width = NULL, height = NULL) {
  ## create widget ----
  htmlwidgets::createWidget(
    name = 'w2ui',
    x = list(
      tabs = tabs
    ),
    width = width,
    height = height,
    package = 'w2ui',
    elementId = inputId
  )
}

#' w2ui tabs output
#'
#' @param outputId output variable to read from
#' @param width
#' @param height
#' @export
tabsOutput <- function(outputId, width = '100%', height = '20px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'tabEditor', width, height, package = 'w2ui')
}

#' w2ui tabs render
#' @export
renderTabs <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, tabsOutput, env, quoted = TRUE)
}


