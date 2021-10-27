library(shiny)
library(listviewer)

safe_list <- function(.list) {
  tryCatch({
    obj <- as.list(.list)
    obj <- lapply(obj, function(x){
      if (is.character(x) && nchar(x) > 300) {
        return(
          paste0(
            substr(x, 1, pmin(nchar(x), 300)),
            "... [[ truncated for space ]]"
          )
        )
      } else {
        return(x)
      }
    })
  }, error = function(e) {
    message(e)
    obj <- list(
      "ERROR",
      e,
      "Please refresh the page to see if the error persists",
      "If so, submit an issue here:",
      "https://github.com/colearendt/shiny-session-info"
    )
  })

  return(obj)
}

ui <- function(req) {
  fluidPage(
    titlePanel("Shiny Session Info"),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("session",
                         jsoneditOutput("sessionInfo")),
                tabPanel("clientData",
                         jsoneditOutput("clientdataText")),
                tabPanel("UI req object",
                         jsonedit(
                           safe_list(req)
                           , mode = 'view'
                           , modes = list('view')
                         ))
    )
  )
)}

server <- function(input, output, session) {

  clean_environ <- function(environ){
    if (is.environment(environ)) {
      lenv <- as.list(environ)
      lenv <- lenv[which(!lapply(lenv, typeof) %in% c("environment"))]
      return(lenv)
    } else {
      return(environ)
    }
  }

  output$sessionInfo <- renderJsonedit({
    tryCatch({
      calt <- as.list(session)

      calt_type <- lapply(calt, typeof)
      calt_clean <- calt[which(!calt_type %in% c("closure"))]
      calt_clean <- lapply(calt_clean, clean_environ)
      calt_class <- lapply(calt_clean, class)
      calt_final <- calt_clean[which(!calt_class %in% c("reactivevalues", "shinyoutput", "list"))]
      # calt_names <- names(calt_final)

      print(lapply(calt_final, typeof))
    },
    error = function(e) {
      message(e)
      calt_final <- list("ERROR occurred", e, "Please refresh the page")
    })

    jsonedit(as.list(calt_final), mode = 'view', modes = list('view'))
  })

  # Values from cdata returned as text
  cdata <- session$clientData
  output$clientdataText <- renderJsonedit({
    jsonedit(as.list(cdata), mode = 'view', modes = list('view'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
