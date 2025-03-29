
ui_sidebar <- function(id="sidebar"){
  ns <- shiny::NS(id)

  bslib::sidebar(
    id = ns("bar"),
    class = "container-sidebar",
    open = TRUE,
    shiny::tags$div(
      class = "container-header"
    ),
    shiny::tags$strong(
      "History",
      class = "header-sidebar"
    ),
    shiny::uiOutput(
      outputId = ns("history"),
      fill = TRUE,
      inline = TRUE
    )
  )
}

server_sidebar <- function(id="sidebar", ide){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      # observe history selection ----
      observeEvent(input$selectHistory, {
        message(input$selectHistory)
        showModal(
          modalDialog(
            title = NULL,
            easyClose = T,
            footer = NULL,
            shiny::tags$code(
              ide$history[["code"]][input$selectHistory]
            )
          )
        )
      })

      # output history ----
      output$history <- shiny::renderUI({
        if(!is.null(ide$history)){
          lapply(seq_along(ide$history[["code"]]), function(x){
            code <- ide$history[["code"]][x]
            time <- ide$history[["start_time"]][x]
            runtime <- ide$history[["run_time"]][x]
            shiny::tags$p(
              class = "p-history",
              code
            ) |>
              bslib::tooltip(paste(paste0("Run:  ", time), paste("Time: ", round(runtime, 5), attributes(runtime)[["units"]]), sep = "\n"))
          })
        }
      })
    }
  )
}
