# is null operator
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

# console output function ----
consoleOutput <- function(evals, .async=F, .src=T, .messages=T, .warnings=T, .errors=T, .output=T){
  if(.async){

  }

  lapply(seq_along(evals), function(chunk){
    src <- shiny::tags$span(
      class = "r-src",
      paste0(
        "> ",
        evals[[chunk]][["src"]]
      )
    )

    co <- c("print(", "str(", "summary(")
    check_co <- lapply(co, function(flag){
      grepl(flag, src, fixed = TRUE)
    })

    if(!.src){
     src <- NULL
    }

    if(.messages){
      if(is.null(evals[[chunk]][["msg"]][["messages"]])){
        messages <- NULL
      }else{
        messages <- shiny::tags$span(class="r-message", paste(evals[[chunk]][["msg"]][["messages"]], collapse = "\n"))
      }
    }else{
      messages <- NULL
    }

    if(.warnings){
      if(is.null(evals[[chunk]][["msg"]][["warnings"]])){
        warnings <- NULL
      }else{
        warnings <- shiny::tags$span(class="r-warning", paste(evals[[chunk]][["msg"]][["warnings"]], collapse = "\n"))
      }
    }else{
      warnings <- NULL
    }

    if(.errors){
      if(is.null(evals[[chunk]][["msg"]][["errors"]])){
        errors <- NULL
      }else{
        errors <- shiny::tags$span(class="r-error", paste(evals[[chunk]][["msg"]][["errors"]], collapse = "\n"))
      }
    }else{
      errors <- NULL
    }


    if(.output){
      if(is.null(evals[[chunk]][["output"]]) & !any(check_co == TRUE)){
        output <- NULL
      }else if(any(check_co == TRUE)){
        output <- shiny::tags$span(
          class="r-output",
          paste(capture.output(eval(parse(text = evals[[chunk]][["src"]]))), collapse = "\n")
        )
      }else{
        output <- shiny::tags$span(class="r-output", paste(evals[[chunk]][["output"]], collapse = "\n"))
      }
    }else{
      output <- NULL
    }

    tgs <- shiny::tagList(
      src,
      if(!is.null(src)){shiny::tags$br()},
      messages,
      if(!is.null(messages)){shiny::tags$br()},
      warnings,
      if(!is.null(warnings)){shiny::tags$br()},
      errors,
      if(!is.null(errors)){shiny::tags$br()},
      output,
      shiny::tags$br()
    )
  })
}

# detect function ----
grep_func <- function(x){
  pattern <- "\\b([a-zA-Z][a-zA-Z0-9._]*)\\s*\\("
  matches <- gregexpr(pattern, x, perl = TRUE)
  function_names <- regmatches(x, matches)[[1]]
  clean <- gsub("\\s*\\($", "", function_names)
  return(clean)
}

# detect object ----
grep_obj <- function(x) {
  pattern <- "\\b([a-zA-Z][a-zA-Z0-9._]*)\\s*$"
  matches <- gregexpr(pattern, x, perl = TRUE)
  clean <- regmatches(x, matches)
  return(clean)
}

# detect r code chunk ----
grep_r <- function(x){
  pattern <- "```\\{r.*?\\}\\s*(.*?)\\s*```"
  matches <- gregexpr(pattern, x, perl = TRUE)
  chunks <- regmatches(x, matches)[[1]]
  clean <- gsub("```\\{r.*?\\}\n|\n```", "", chunks)
  return(clean)
}

# detect assignment ----
grep_assignment <- function(x) {
  arrow <- sub("<-.*", "", x)
  if(nzchar(arrow) & arrow != x){
    arrow <- trimws(arrow)
    return(arrow)
  }
  equal <- sub("=.*", "", x)
  if(nzchar(equal) & equal != x){
    equal <- trimws(equal)
    if(grepl("[()]", equal)){
      return(NA)
    }
    return(equal)
  }
  return(NA)
}

# find render function ----
find_render_func <- function(f){
  sapply(f, function(func){
    package <- find(func)
    package_funs <- ls(package)
    render_fun <- paste0(
      gsub("package:", "", package, fixed = TRUE),
      "::",
      package_funs[grep("render", package_funs, ignore.case = TRUE)]
    )
  }) |>
    as.character()
}

# find object render function ----
find_obj_render_func <- function(o){
  obj_class <- lapply(o, function(obj){
    obj_get <- class(get(obj, .GlobalEnv))
    if(any(obj_get == "htmlwidget")){
      obj_get <- obj_get[!obj_get %in% "htmlwidget"]
      package_funs <- ls(paste0("package:", obj_get))
      render_fun <- paste0(
        obj_get,
        "::",
        package_funs[grep("render", package_funs, ignore.case = TRUE)]
      )
    }
  }) |>
    as.character()
}

# viewer output function ----
viewerOutput <- function(run){
  viewer <- tryCatch(
    render_funs <- lapply(seq_along(run), function(v){
      funs <- grep_func(run[[v]][["src"]])
      if(any(run[[v]][["type"]] %in% c("htmlwidget"))){
        fun <- find_render_func(funs)
        if(length(fun) == 0L){
          fun <- find_obj_render_func(run[[v]][["src"]])
        }
        if(length(fun) > 0L){
          rerun_code <- paste0(fun[1], "({", run[[v]][["src"]], "})")
          run_code <- evals(txt = rerun_code, env = .GlobalEnv)
          run_code[[1]][["result"]]
        }
      }else if(any(run[[v]][["msg"]][["errors"]] == "'browser' must be a non-empty character string")){
        fun <- find_render_func(funs)
        if(length(fun) == 0L){
          fun <- find_obj_render_func(run[[v]][["src"]])
        }
        if(length(fun) > 0L){
          rerun_code <- paste0(fun[1], "({", run[[v]][["src"]], "})")
          run_code <- evals(txt = rerun_code, env = .GlobalEnv)
          run_code[[1]][["result"]]
        }
      }else if(any(run[[v]][["type"]] %in% "image")){
        if(any(funs %in% c("ggplot", "plot"))){
          fun <- "shiny::renderPlot"
          rerun_code <- paste0(fun, "({", run[[v]][["src"]], "})")
          run_code <- evals(txt = rerun_code, env = .GlobalEnv)
          run_code[[1]][["result"]]
        }
      }else if(any(run[[v]][["type"]] %in% c("shiny.tag", "html", "shiny.tag.list"))){
        fun <- "shiny::renderUI"
        rerun_code <- paste0(fun, "({", run[[v]][["src"]], "})")
        run_code <- evals(txt = rerun_code, env = .GlobalEnv)
        run_code[[1]][["result"]]
      }
    }),
    error = function(e){
      NULL
    }
  )

  if(!is.null(viewer)){
    viewer <- Filter(Negate(is.null), viewer)
    if(length(viewer) > 0){
      return(viewer)
    }
  }

  return(NULL)
}

# reactable buttons ----
reactable_button <- function(inputId, icon) {
  paste0(
    "
  function(cellInfo, state) {
    var clickid = '", inputId, "';
    var { index } = cellInfo;
    return `<i class='", icon, " reactable-bttn' style='color:red;' ` +
      `id='${index+1}' ` +
      `onclick='event.stopPropagation(); Shiny.setInputValue(&#39;${clickid}&#39;, this.id, {priority: &#39;event&#39;})' ` +
      `style='padding-left: 0.2em; flex: 0 0 auto;min-width: 45px;width: 45px;max-width: 45px;text-overflow: clip;user-select: none;'></i>`
  }
  "
  ) |> htmlwidgets::JS()
}

# get all tables ----
get_tables <- function(){

}

# get all envrionment objects ----
get_environment <- function(){
  environment <- ls(.GlobalEnv)
  class <- as.character(lapply(mget(environment, envir = .GlobalEnv), class))
  df_environment <- data.frame(
    Object = environment,
    Class = class
  )
  if(nrow(df_environment) > 0){
    df_environment$trash <- NA_character_
    df_environment$Created <- uuid::UUIDgenerate()
  }else{
    df_environment$trash <- character(0)
    df_environment$Created <- character(0)
  }
  return(df_environment)
}

# remove object from envrionment ----
remove_environment <- function(object){
  rm(list = object, envir = .GlobalEnv)
  gc()
}

# get all packages ----
get_packages <- function(){
  df_package <- sessionInfo()[["otherPkgs"]]
  df_package <- do.call(rbind, lapply(seq_along(df_package), function(x) {
    data.frame(
      Package = df_package[[x]][["Package"]],
      Title = df_package[[x]][["Title"]],
      Version = df_package[[x]][["Version"]]
    )
  }))
  return(df_package)
}

# response from duckdb ----
duckdb_response <- function(result, environment){
  pending <- environment[environment$Class == "Pending Query", ]

  if(nrow(pending) == 0L){
    return(NULL)
  }

  id <- NULL
  for (i in 1:nrow(pending)) {
    object <- pending[["Object"]][i]
    object_id <- tryCatch(
      get(pending[["Object"]][i]),
      error = function(e) NULL
    )
    req(object_id)
    query_id <- result[["id"]]
    if(object_id == query_id){
      assign(object, result, envir = .GlobalEnv)
      id <- object_id
      break
    }
    next
  }

  if(is.null(id)){
    return(NULL)
  }

  return(
    list(
      list(
        src = result[["query"]],
        result = NULL,
        output = if(is.null(id)){
          paste(capture.output(eval(parse(text = result))), collapse = "\n")
        }else{
          NULL
        },
        type = "duckdb_result",
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
  )
}

# check object ----
check_object_type <- function(obj) {
  cls <- class(obj)
  if (is.null(obj)) {
    return("NULL")
  } else if ("data.frame" %in% cls) {
    return("data.frame")
  } else if ("matrix" %in% cls) {
    return("matrix")
  } else if ("array" %in% cls & !"matrix" %in% cls) {
    return("array")
  } else if ("list" %in% cls & !"data.frame" %in% cls) {
    return("list")
  } else if ("factor" %in% cls) {
    return("factor")
  } else if ("function" %in% cls) {
    return("function")
  } else if ("tbl" %in% cls) {
    return("tibble")
  } else if ("environment" %in% cls) {
    return("environment")
  } else if ("name" %in% cls) {
    return("symbol")
  } else if ("expression" %in% cls) {
    return("expression")
  } else if ("Date" %in% cls) {
    return("Date")
  } else if ("POSIXct" %in% cls || "POSIXlt" %in% cls) {
    return("datetime")
  } else if ("complex" %in% cls) {
    return("complex")
  } else if ("raw" %in% cls) {
    return("raw")
  } else if ("formula" %in% cls) {
    return("formula")
  } else if ("ts" %in% cls) {
    return("time series")
  } else if ("data.table" %in% cls) {
    return("data.table")
  } else if ("Matrix" %in% cls) {
    return("sparse matrix")
  } else if (is.vector(obj)) {
    return("vector")
  } else {
    return("other")
  }
}

# show notification ----
show_notification <- function(type, msg, duration=5, id){
  if(type == "error"){
    shiny::showNotification(
      ui = shiny::tags$div(
        style = "display: flex; align-items: center;",
        shiny::tags$i(
          class = "fas fa-circle-xmark",
          style = "color: #ff0000; margin-right: 0.5em;"
        ),
        shiny::tags$strong(msg),
      ),
      type = "warning",
      id = id,
      duration = duration
    )
  }else if(type == "success"){
    shiny::showNotification(
      ui = shiny::tags$div(
        style = "display: flex; align-items: center; gap: 0.25;",
        shiny::tags$span(
          class = "notification-left",
          shiny::tags$i(
            class = "fas fa-circle-check notification-check"
          )
        ),
        shiny::tags$div(
          shiny::tags$strong(msg)
        )
      ),
      id = id,
      type = "message",
      duration = duration
    )
  }else if(type == "loading"){
    shiny::showNotification(
      ui = shiny::tags$div(
        style = "display: flex; align-items: center; gap: 0.25;",
        shiny::tags$span(
          class = "notification-left notification-spinner"
        ),
        shiny::tags$div(
          shiny::tags$strong(msg)
        )
      ),
      id = id,
      type = "message",
      duration = duration
    )
  }
}

# eval code by ext ----
eval_code <- function(code, ext, session = shiny::getDefaultReactiveDomain()){
  # .R, .RMD, .QMD, .MD, .APP, .API, .DB, .JS or .SQL
  ext <- tolower(ext)
  if(ext %in% c("r")){
    tic <- Sys.time()
    run <- evals(txt = code, env = .GlobalEnv)
    viewer <- viewerOutput(run)
    toc <- Sys.time()
    runtime <- toc - tic
    id <- uuid::UUIDgenerate()
    return(
      list(
        start_time = format(tic, "%Y-%m-%d %I:%M:%S %p"),
        run_time = runtime,
        id = id,
        code = code,
        evals = run,
        viewer = viewer
      )
    )
  }else if(ext %in% c("md")){
    tic <- Sys.time()
    viewer <- html_2_R(
      html = shiny::markdown(code),
      out = NULL
    )
    toc <- Sys.time()
    runtime <- toc - tic
    id <- uuid::UUIDgenerate()
    return(
      list(
        start_time = format(tic, "%Y-%m-%d %I:%M:%S %p"),
        run_time = runtime,
        id = id,
        code = code,
        evals = run,
        viewer = viewer
      )
    )
  }else if(ext %in% c("rmd", "qmd")){
    tic <- Sys.time()
    md <- shiny::markdown(code)
    rmd <- xml2::read_html(md)
    run <- rmd |>
      xml2::xml_find_all("//code") |>
      xml2::xml_text()
    run <- lapply(run, function(eval){
      evals(txt = eval, env = .GlobalEnv)
    })
    out <- lapply(seq_along(run), function(i){
      node <- run[[i]]
      if(any(node[[1]][["type"]] == "htmlwidget")){
        v <- viewerOutput(node)
      }else if(any(node[[1]][["msg"]][["errors"]] == "'browser' must be a non-empty character string")){
        v <- viewerOutput(node)
      }else{
        v <- consoleOutput(node, .src = FALSE)
      }
    })
    viewer <- html_2_R(
      html = md,
      out = out
    )
    toc <- Sys.time()
    runtime <- toc - tic
    id <- uuid::UUIDgenerate()
    return(
      list(
        start_time = format(tic, "%Y-%m-%d %I:%M:%S %p"),
        run_time = runtime,
        id = id,
        code = code,
        evals = run,
        viewer = viewer
      )
    )
  }else if(ext %in% c("app")){

  }else if(ext %in% c("api")){

  }else if(ext %in% c("sql", "db")){
    code <- gsub("(^\n+|\n+$)", "", code)
    id <- uuid::UUIDgenerate()
    tic <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    session$sendCustomMessage(
      "duckdb_sql",
      list(
        sql = code,
        id = id
      )
    )
    evals <- list(
      list(
        src = code,
        result = NULL,
        output = NULL,
        type = "duckdb_result",
        msg = list(
          messages = paste("Pending Query ID:", id),
          warnings = NULL,
          errors = NULL
        )
      )
    )
    return(
      list(
        start_time = tic,
        run_time = tic,
        id = id,
        code = code,
        evals = evals,
        viewer = NULL
      )
    )
  }else if(ext %in% c("js")){

  }
}

make_attrs <- function(node) {
  attrs <- xml_attrs(node)
  as.list(attrs)
}

render_node <- function(node, prefix = FALSE) {
  if (xml_type(node) == "text") {
    txt <- trimws(xml_text(node))
    if (nchar(txt) > 0) {
      return({
        paste0('\n', txt)
      })
    }
    return(NULL)
  } else {
    tagName <- if (prefix) paste0("tags$", xml_name(node)) else xml_name(node)
    tag_func <- eval(parse(text = tagName))

    attrs <- make_attrs(node)
    children <- xml_contents(node)
    child_content <- lapply(children, function(child) {
      render_node(child, prefix = prefix)
    })
    child_content <- Filter(Negate(is.null), child_content)

    do.call(tag_func, c(attrs, child_content))
  }
}

html_2_R <- function(html, path = "//body/*", prefix = TRUE, out=NULL) {
  doc <- read_html(html)
  nodes <- xml_find_all(doc, path)
  if(length(nodes) == 0){
    return(NULL)
  }

  o <- 0
  rmd <- tagList()
  for (n in seq_along(nodes)) {
    node <- render_node(nodes[[n]], prefix = prefix)
    if(node[["name"]] == "pre"){
      o <- o + 1
      node <- tagList(
        node,
        out[[o]]
      )
    }
    rmd[[n]] <- node
  }
  shiny::tagList(
    shiny::fluidPage(
      rmd
    )
  )
}

focusHandsontable <- function(id){
  shinyjs::runjs(
    paste0(
      "const element = document.getElementById('", id, "');",
      "element.hot.selectCell(0,0)"
    )
  )
}
