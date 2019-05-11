getText <- function(data) {
  # This function takes  a data table with type, author, title, and URL, and then returns the text of that poem
  #
  # Args:
  #   data    A data table with type, author, title, and URL
  # Returns:
  #   A vector of text from peom in the data
  # Side-effects:
  #   None
  author <- data$author
  title <- data$title
  dest <- glue('./poems/{file}', file = glue('{title}-{author}.html'))
  text <- character()
  for (f in dest) {
    if (!file.exists(f)) {
      stop(glue('Cannot find ',f,'.\nPlease verify poems are stored under `poems` directory.\n'))
    }
    newtext <-
      read_html(f, encoding = 'UTF-8') %>%
      html_nodes('div.poem p') %>%
      html_text() %>%
      # must first remove new-line character so the other regex
      # would be correctly applied to multi-lines
      str_remove_all('\\n') %>%
      str_remove_all('〈.+?〉|\\[.+?\\]| |　|，|。|？|！|；')
    text <- c(text, newtext)
  }
  return(text)
}

getLastChar <- function(data) {
  # This function takes a data table with text of poems, and then returns the text of that poem
  #
  # Args:
  #   data    A data table with text of poems
  # Returns:
  #   A vector consists of last characters of each sentence of a poem
  # Side-effects:
  #   None
  idx <- function(type) {
    switch(type,
           Wulü  = seq(10, 40, 10),
           Qilü  = seq(14, 56, 14),
           Wujue = seq(10, 20, 10),
           Qijue = seq(14, 28, 14))
  }
  pmap(data[,.(type, text)],
       function(type, text) {
         nextChar <- character()
         for (i in idx(type)) {
           nextChar <- c(nextChar, str_sub(text, i, i))
         }
         nextChar <- paste0(nextChar, collapse = '')
       }
  )
}

processPoems <- function(DT) {
  # Add a new column with the text
  DT[ , `:=` ('text' = getText(.SD))]
  # Add a new column with the last characters
  DT[ , `:=` ('lastChar' = getLastChar(.SD))]
}

processPoems(DT)
