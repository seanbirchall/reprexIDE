# shiny ----
library(shiny)
library(bslib)
library(shinyAce)
library(shinyjs)
# data ----
library(jsonlite)
library(xml2)
library(dplyr)
library(tidyr)
library(styler)
# visual ----
library(reactable)
library(htmlwidgets)
# eval ----
library(pander)
library(uuid)
# opts ----
pander::cache.off()
styler::cache_deactivate()
# functions ----
scrapeable <- new.env()

with(scrapeable, {
  get_data <- function(db=NULL, tbl=NULL, query=NULL, limit=1000, session=shiny::getDefaultReactiveDomain()){
    if(any(is.null(db), is.null(tbl))){
      stop("db, tbl, and query required")
    }
    url <- get_db(
      db = db
    )
    if(!is.null(query)){
      .tbl <- paste("sc", tbl, sep = ".")
      .limit <- grepl("limit", query, ignore.case = T)
      .select <- grepl("select", query, ignore.case = T)
      query <- gsub(tbl, .tbl, query)
      if(!.limit & .select){
        query <- paste(
          query, "LIMIT", limit
        )
      }
    }else{
      # dbplyr::lazy_frame()
      message("lazy eval")
    }
    id <- uuid::UUIDgenerate()
    session$sendCustomMessage(
      "duckdb_r",
      list(
        url = url,
        query = query,
        id = id
      )
    )
    class(id) <- "Pending Query"
    message("Awaiting query results...")
    return(id)
  }

  get_db <- function(db=NULL){
    if(!is.null(db)){
      # return("https://scrapeable-public.nyc3.digitaloceanspaces.com/test.duckdb")
      return("s3://duckdb-wasm-test/6e6-idx.duckdb")
      # return("https://scrapeable-data.01bec96ddf135b4f6636692059641ffe.r2.cloudflarestorage.com/db_test.duckdb?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=f7f92bbce532f6ead44c019c43921a18%2F20240916%2Fauto%2Fs3%2Faws4_request&X-Amz-Date=20240916T161336Z&X-Amz-Expires=3600&X-Amz-SignedHeaders=host&X-Amz-Signature=3414679ce29d2c894c7f4771c1b4537a74db31ba4221cacb60c8a3286689fbd6")
    }
  }

  is_character <- function(x){
    tryCatch(
      is.character(x),
      error = function(e) FALSE
    )
  }

  await <- function(obj, class = "Pending Query", interval = 0.5, timeout = 30) {
    start_time <- Sys.time()

    while (TRUE) {
      # Check if the object exists in the environment
      if (!exists(obj, envir = .GlobalEnv)) {
        stop(paste("Object", obj, "not found in the global environment"))
      }

      # Get the current class of the object
      current_class <- class(get(obj, envir = .GlobalEnv))

      # Check if the class has changed
      if (!identical(current_class, class)) {
        message(paste("Object", obj, "changed from", class, "to", current_class))
        return(invisible(current_class))
      }

      # Check if we've exceeded the timeout
      if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
        stop(paste("Timeout reached. Object", obj, "did not change class within", timeout, "seconds"))
      }

      # Sleep for the specified interval
      Sys.sleep(interval)
    }
  }

  get_new <- function(){

  }

  get_proxy <- function(){

  }

  query <- function(db, query, session=getDefaultReactiveDomain()){

  }

  proxy <- function(url, server="https://www.scrapeable.com/"){

  }

  rfetch <- function(url, ...){

  }

  run_app <- function(app){

  }

  shiny_app <- function(ui, server){

  }

  render <- function(rmd){

  }

  view <- function(obj=NULL, session=shiny::getDefaultReactiveDomain()){
    if(is.null(obj)){
      obj <- "matrix(nrow = 50, ncol = 26) |> data.frame() |> setNames(LETTERS)"
      message("No Object: returning empty spreadsheet")
    }
    if(!is_character(obj)){
      obj <- deparse(substitute(obj))
    }
    shinyjs::delay(1, shinyjs::click(id = "control-tab_viewer", asis = TRUE))
    shinyjs::delay(1, shinyjs::click(id = "control-viewer-show", asis = TRUE))
    session$sendCustomMessage(
      "view",
      list(
        obj = obj
      )
    )
  }
})

attach(scrapeable, name = "package:scrapeable")
