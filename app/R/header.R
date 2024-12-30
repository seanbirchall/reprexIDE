ui_header <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "header-search-container-left",
      shiny::tags$img(
        src = "logo2.svg",
        height = "24px",
        width = "24px"
      ),
      shiny::tags$div(
        class = "dropdown",
        id = "fileDropdown",
        onmouseover = "this.style.backgroundColor='#eee'",
        onmouseout = "this.style.backgroundColor='rgb(246, 247, 249)'",
        shiny::tags$a(
          "File",
          class = "dropdown-button",
          onclick = "toggleDropdown(event)"
        ),
        shiny::tags$div(
          id = "fileDropdownContent",
          class = "dropdown-content",
          shiny::tags$a(
            "New File +",
            class = "dropdown-content-item",
            onclick = "handleMenuClick('newFile')"
          ),
          shiny::tags$a(
            "New Project +",
            class = "dropdown-content-item",
            onclick = "handleMenuClick('newProject')"
          ),
          shiny::tags$a(
            "Load Project",
            class = "dropdown-content-item",
            onclick = "handleMenuClick('loadProject')"
          ),
          shiny::tags$a(
            "Shortcuts",
            class = "dropdown-content-item",
            onclick = "handleMenuClick('shortcuts')"
          )
        )
      )
    ),
    shiny::tags$div(
      class = "header-search-container-center"
      # shiny::selectizeInput(
      #   inputId = "search",
      #   label = NULL,
      #   width = "100%",
      #   multiple = TRUE,
      #   choices = NULL,
      #   options = list(
      #     # valueField = "id",
      #     # labelField = "label",
      #     # searchField = c("id", "name", "description", "tag"),
      #     placeholder = "🔍 Search for code snippets.... (coming soon!)",
      #     maxOptions = 10,
      #     maxItems = 1,
      #     create = TRUE
      #   )
      # ) |>
      #   htmltools::tagAppendAttributes(class = "header-search-selectize")
    ),
    shiny::tags$div(
      class = "header-search-container-right",
      shiny::actionButton(
        inputId = ns("deploy"),
        label = "Deploy",
        style = "width: 80px; font-size: 12px; padding: 0px; border: 1px solid rgb(246, 247, 249); background-color: rgb(246, 247, 249); color: black; margin-left: 5px;",
        icon = shiny::icon(
          "cloud-arrow-up"
        ),
        onmouseover = "this.style.backgroundColor='#eee'",
        onmouseout = "this.style.backgroundColor='rgb(246, 247, 249)'"
      ) |>
        bslib::tooltip("Cloud Deploy", placement = "bottom"),
      shiny::actionButton(
        inputId = ns("share"),
        label = "Share",
        style = "width: 80px; font-size: 12px; padding: 0px; border: 1px solid rgb(246, 247, 249); background-color: rgb(246, 247, 249); color: black;",
        icon = shiny::icon(
          "share-nodes"
        ),
        onmouseover = "this.style.backgroundColor='#eee'",
        onmouseout = "this.style.backgroundColor='rgb(246, 247, 249)'"
      ) |>
        bslib::tooltip("Create Link", placement = "bottom"),
      shiny::actionButton(
        inputId = ns("login"),
        label = NULL,
        icon = shiny::icon(
          "user"
        ),
        class = "button-login"
      ) |>
        bslib::tooltip("Login / Sign Up", placement = "bottom")
    )
  )
}

server_header <- function(id, ide){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      # show login ----
      observeEvent(input$login, {
        ide$show_login <- ide$show_login + 1
      })

      # observe share link ----
      shiny::observeEvent(input$share, {
        if(session$userData$logged_in){
          ## share notification ----
          show_notification(
            type = "loading",
            msg = "Preparing Share Link",
            duration = 30,
            id = "notification_share"
          )
          ## share tabs ----
          ide$tabs[[ide$tab_selected]][["code"]] <- input[["editor-ace"]]
          tabs <- ide$tabs
          tabs <- jsonlite::base64url_enc(
            serialize(
              object = tabs,
              connection = NULL
            )
          )
          ## check new code ----
          if(!identical(tabs, ide$last_tabs)){
            ide$last_tabs <- tabs
            id <- uuid::UUIDgenerate()
            ide$last_id <- id
            payload <- list(
              code = tabs,
              id = id
            )
          }else{
            id <- ide$last_id
            payload <- NULL
          }
          ## send payload if not null ----
          if(!is.null(payload)){
            session$sendCustomMessage(
              "put_code",
              list(
                inputId = "put_code_response",
                payload = payload
              )
            )
          }
        }else{
          ## unauthenticated share ----
          shinyjs::click("login")
          shiny::removeNotification(
            id = "notification_login",
          )
          show_notification(
            type = "error",
            msg = "Please Login to Share",
            duration = 5,
            id = "notification_login"
          )
        }
      })

      # put code response ----
      shiny::observeEvent(input$put_code_response, {
        ide$put_code_response <- input$put_code_response
      })
      shiny::observeEvent(ide$put_code_response, {
        response <- ide$put_code_response
        if(response$status == "success"){
          removeNotification(id = "notification_share")
          show_notification(
            type = "success",
            msg = "Share Link Created",
            duration = 3,
            id = "notification_share_success"
          )
          # You can handle the successful response data here
          # response$data will contain what your Lambda returned
        }else{
          removeNotification(id = "notification_share")
          show_notification(
            type = "error",
            msg = "Failed to Create Share Link",
            duration = 3,
            id = "notification_share_error"
          )
        }
      })
    }
  )
}
