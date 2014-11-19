# http://stackoverflow.com/a/14452990/2954547

inputTextArea <- function(inputId, value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(
      src = "textArea.js"))),
    tags$textarea(id = inputId,
                  class = "inputtextarea",
                  rows = nrows,
                  cols = ncols,
                  as.character(value))
  )
}
