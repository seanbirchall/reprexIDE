# ui function ----
ui <- bslib::page(

  ## global ----
  shinyjs::useShinyjs(),
  title = "REPREX IDE",
  theme = bslib::bs_theme(
    preset = "flatly",
    secondary = "#f3969a",
    font_scale = 0.95,
    `enable-rounded` = FALSE,
    `enable-transitions` = FALSE
  ),

  ## css ----
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "handsontable.css"),
    tags$script(type = "module", src = "duckdb.js"),
    shiny::tags$script(type = "text/javascript", src = "handsontable.js"),
    shiny::tags$script(type = "text/javascript", src = "df_viewer2.js"),
    # js to load right away ----
    shiny::tags$script(type = "text/javascript", src = "view.js"),
    shiny::tags$script(type = "text/javascript", src = "dropdown.js"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "split.css"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "w2ui.css"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "w2ui-2.0.min.css")
  ),

  ## hidden ----
  shiny::actionButton(
    inputId = "style",
    label = NULL,
    style = "display: none;"
  ),
  shiny::actionButton(
    inputId = "logged_in",
    label = NULL,
    style = "display: none;"
  ),

  ## content ----
  bslib::card(
    class = "main-container",
    full_screen = FALSE,
    fill = TRUE,
    bslib::card_header(
      class = "header-search",
      ui_header(
        id = "header"
      )
    ),
    bslib::layout_sidebar(
      class = "container-sidebar-layout",
      sidebar = ui_sidebar(
        id = "sidebar"
      ),
      shiny::tags$div(
        class = "split",
        shiny::tags$div(
          id = "pane-code",
          shiny::tags$div(
            id = "pane-editor",
            ui_editor(
              id = "editor"
            )
          ),
          shiny::tags$div(
            id = "pane-console",
            ui_console(
              id = "console"
            )
          )
        ),
        shiny::tags$div(
          id = "pane-control",
          ui_control(
            id = "control"
          )
        )
      )
    )
  ),

  ## js ----
  shiny::tags$footer(
    shiny::tags$script(type = "text/javascript", src = "split.min.JS"),
    shiny::tags$script(type = "text/javascript", src = "split.js"),
    shiny::tags$script(type = "text/javascript", src = "w2ui-2.0.min.js"),
    shiny::tags$script(type = "text/javascript", src = "w2ui.js"),
    shiny::tags$script(type = "text/javascript", src = "handlers.js"),
    shiny::tags$script(type = "text/javascript", src = "utils.js"),
    shiny::tags$script(type = "text/javascript", src = "events.js"),
    shiny::tags$script(type = "text/javascript", src = "keyboard_shortcuts.js")
  )
)

# server function ----
server <- function(input, output, session) {

  ## on-load ide ----
  ide <- shiny::reactiveValues(
    tabs = list(
      tab1 = list(
        name = "script.R",
        code = ""
      )
    ),
    tabs_available = "tab1",
    tab_selected = "tab1",
    tab_previous = "tab1",
    tab_selected_extension = "r",
    history = NULL,
    last_run = "",
    last_code = "no-hash",
    show_login = 1,
    show_share = 1,
    show_notification = 1,
    show_df_viewer = FALSE,
    environment = data.frame(
      Object = character(0),
      Class = character(0)
    )
  )

  ## sub-modules ----
  server_header(
    id = "header",
    ide = ide
  )
  server_sidebar(
    id = "sidebar",
    ide = ide
  )
  server_modal(
    id = "modal",
    ide = ide
  )
  server_editor(
    id = "editor",
    ide = ide
  )
  server_console(
    id = "console",
    ide = ide
  )
  server_control(
    id = "control",
    ide = ide
  )

  ## check auth on-load set logged in ----
  session$sendCustomMessage(
    "check_refresh_token",
    list(
      inputId = "tokens"
    )
  )
  shiny::observeEvent(input$tokens, {
    if(input$tokens[["status"]] == "success"){
      session$userData$logged_in <- TRUE
      return()
    }
    session$userData$logged_in <- FALSE
  })

  ## recheck login ----

  ## style code ----
  shiny::observeEvent(input$style, {
    code <- gsub("\r\n", "\n", input[["editor-ace"]])
    code <- tryCatch(
      styler::style_text(text = code),
      error = function(e){
        return(code)
      }
    )
    code <- paste(code, collapse = "\n")
    ide$tabs[[ide$tab_selected]][["code"]] <- code
    shinyAce::updateAceEditor(
      session = session,
      editorId = "editor-ace",
      value = ide$tabs[[ide$tab_selected]][["code"]]
    )
  })

  ## extension ----
  observeEvent(ide$tab_selected, {
    ide$tab_selected_extension <- tolower(
      tools::file_ext(ide$tabs[[ide$tab_selected]][["name"]])
    )
  })

  ## selected tab ----
  observeEvent(input$tab, {
    tab_new <- input$tab
    ide$tab_selected <- tab_new
    ### save previous tab code ----
    if(!is.null(ide$tab_previous)){
      ide$tabs[[ide$tab_previous]][["code"]] <- input[["editor-ace"]]
    }
    ### create new tab ----
    if(!tab_new %in% names(ide$tabs)){
      ide$tabs[[tab_new]][["code"]] <- ""
      tab_name <- paste0("script", gsub("[^0-9]", "", tab_new), ".R")
      ide$tabs[[tab_new]][["name"]] <- tab_name
      ide$tabs_available <- c(ide$tabs_available, tab_new)
    }
    ### Update Ace with code ----
    updateAceEditor(
      session = session,
      editorId = "editor-ace",
      value = ide$tabs[[tab_new]][["code"]]
    )
    ide$tab_previous <- tab_new
  })

  ## tab close ----
  observeEvent(input$tab_close, {
    tab_to_close <- input$tab_close
    ide$tabs_available <- ide$tabs_available[!ide$tabs_available %in% tab_to_close]
    ide$tabs <- ide$tabs[ide$tabs_available]
    if(!is.null(input$tab)){
      if(tab_to_close == input$tab){
        tab_selected <- names(ide$tabs)[1]
        if(is.na(tab_selected)){
          shinyjs::click(
            id = "tabs_tabs_tab_add"
          )
        }else{
          shinyjs::click(
            id = paste0("tabs_tabs_tab_", tab_selected)
          )
        }
      }
    }else{
      shinyjs::click(
        id = "tabs_tabs_tab_add"
      )
    }
  })

  ## tab edited ----
  shiny::observeEvent(input$tab_edit, {
    tab_edit <- input$tab_edit
    tab_edited <- tab_edit[["tabId"]]
    tab_edited_name <- tab_edit[["newName"]]
    ide$tab_selected_extension <- tolower(
      tools::file_ext(tab_edited_name)
    )
    if(tab_edited %in% ide$tabs_available){
      ide$tabs[[tab_edited]][["name"]] <- tab_edited_name
    }
  })

  ## on start observe query parameters ----
  shiny::observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)

    ### initialize tabs on app start ----
    if(!is.null(query[['ide']])){
      if(nchar(query[['ide']]) == 36){
        lookup <- tryCatch(
          jsonlite::fromJSON(
            paste0(
              "https://scrapeable-share.nyc3.cdn.digitaloceanspaces.com/",
              query[['ide']],
              ".json"
            )
          ),
          error = function(e){
            NULL
          }
        )
        if(!is.null(lookup)){
          tabs <- lookup[["code"]]
          tabs <- unserialize(
            jsonlite::base64url_dec(
              tabs
            )
          )
          names(tabs) <- paste0("tab", seq_along(names(tabs)))
          ide$tabs <- tabs
          ide$tabs_available <- names(tabs)
          name <- unlist(ide$tabs)
          name <- as.character(name[grep("\\.name$", names(name))])
          session$sendCustomMessage(
            "initialize_tabs",
            list(
              tabs = name
            )
          )
          shinyAce::updateAceEditor(
            session = session,
            editorId = "editor-ace",
            value = ide$tabs[["tab1"]][["code"]]
          )
        }else{
          show_notification(
            type = "error",
            msg = "Bad Share Link",
            duration = 5,
            id = "notification_link"
          )
        }
      }else{
        show_notification(
          type = "error",
          msg = "Bad Share Link",
          duration = 5,
          id = "notification_link"
        )
      }
    }
  })

  ## df_viewer modal ----
  shiny::observeEvent(input$modal_df_viewer, {
    ide$modal_df_viewer <- input$modal_df_viewer
  })

  ## duckdb ----
  shiny::observeEvent(input$duckdb_r_result, {
    evals <- duckdb_response(
      result = input$duckdb_r_result,
      environment = ide$environment
    )

    if(is.null(evals)){
      result <- input$duckdb_r_result
      id <- result[["id"]]
      evals <- list(
        list(
          src = result[["query"]],
          result = NULL,
          output = paste(capture.output(glimpse(result)), collapse = "\n"),
          type = "duckdb_r_result",
          msg = list(
            messages = if(result$message == "success") paste("Query ID:", id, "ran successfully") else NULL,
            warnings = NULL,
            errors = if(result$message == "success"){
              NULL
            }else{
              if(jsonlite::validate(result$message)){
                jsonlite::fromJSON(result$message)
              }else{
                result$message
              }
            }
          )
        )
      )
    }
    ide$evals <- evals
    ide$environment <- get_environment()
  })

  ## duckdb sql ----
  observeEvent(input$duckdb_sql_result, {
    result <- input$duckdb_sql_result
    evals <- list(
      list(
        src = result[["query"]],
        result = NULL,
        output = NULL,
        type = "duckdb_sql_result",
        msg = list(
          messages = if(result[["message"]] == "success") paste("Query ID:", result[["id"]], "ran successfully") else NULL,
          warnings = NULL,
          errors = if(result[["message"]] == "success"){
            NULL
          }else{
            if(jsonlite::validate(result[["message"]])){
              jsonlite::fromJSON(result[["message"]])
            }else{
              result[["message"]]
            }
          }
        )
      )
    )
    ide$environment_selected[["data"]] <- tryCatch(
      fromJSON(result[["data"]]),
      error = function(e) NULL
    )
    ide$evals <- evals
    ide$viewer_window[["type"]] <- "df_viewer"
    ide$viewer_window[["id"]] <- uuid::UUIDgenerate()
  })

  ## view shim ----
  shiny::observeEvent(input$view, {
    code <- input$view[["object"]]
    parse <- evals(code)
    l <- max(length(parse), na.rm = T)
    result <- parse[[l]][["result"]]
    check_type <- check_object_type(result)
    if(check_type %in% c("data.frame", "matrix", "tibble", "data.table")){
      ide$environment_selected[["data"]] <- result
    }
  })
}

# run app ----
shinyApp(ui = ui, server = server)
