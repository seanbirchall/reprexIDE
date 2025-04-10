server_modal <- function(id="modal", ide){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      # sub-modules ----
      server_modal_df_viewer(
        id = "modal_df_viewer",
        ide = ide
      )

      # login ----
      shiny::observeEvent(ide$show_login, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Login / Sign Up",
            shiny::column(
              width = 12,
              shiny::fluidRow(
                class = "justify-content-center",
                shiny::actionButton(
                  inputId = ns("sign_in"),
                  label = "Login",
                  style = "width: 50%;",
                  onClick = "window.parent.location.href='https://reprex-org.auth.us-east-2.amazoncognito.com/login?client_id=4agp589aqu7nu1fdog4pav4gpg&redirect_uri=https://reprex.org/auth/callback&response_type=code&scope=openid';",
                )
              ),
              shiny::tags$br(),
              shiny::fluidRow(
                class = "justify-content-center",
                shiny::actionButton(
                  inputId = ns("sign_up"),
                  label = "Sign Up",
                  style = "width: 50%;",
                  onClick = "window.parent.location.href='https://reprex-org.auth.us-east-2.amazoncognito.com/signup?client_id=4agp589aqu7nu1fdog4pav4gpg&redirect_uri=https%3A%2F%2Freprex.org%2Fauth%2Fcallback&response_type=code&scope=openid';",
                )
              )
            ),
            footer = "redirecting to AWS Cognito",
            easyClose = TRUE,
            size = "m"
          )
        )
      }, ignoreInit = TRUE)

      # share ----
      observeEvent(ide$show_share, {
        shiny::showModal(
          shiny::modalDialog(
            title = "IDE URL",
            shiny::fluidRow(
              shiny::column(
                width = 9,
                shiny::textInput(
                  inputId = ns("share_text"),
                  label = NULL,
                  width = "100%",
                  value = paste0("https://www.scrapeable.com/webR/?ide=", ide$last_id)
                )
              ),
              shiny::column(
                width = 3,
                shiny::actionButton(
                  inputId = ns("share_ide"),
                  label = "Copy URL",
                  icon = shiny::icon(
                    "link"
                  ),
                  onclick = paste0("copy_by_id('modal-share_text', false)"),
                  style = "width: 150px;"
                )
              )
            ),
            easyClose = TRUE,
            footer = NULL,
            size = "xl"
          )
        )
      }, ignoreInit = TRUE)
    }
  )
}
