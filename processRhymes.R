getRhymes <- function(data) {
  # This function takes a list/column of data table, and then generate another data table with vowels of those characters
  #
  # Args:
  #   data    A list or a column of a data table containing rhyming characters
  # Returns:
  #   A data table consists of the vowels of those rhyming characters
  # Side-effects:
  #   None
  DTRhymes <- data.table(char = character(), rhymes = character())

  vocabulary <-
    str_split(data, '') %>%
    unlist() %>%
    unique()

  # Only take the first vowel if there're multiple available
  # Return NA if it's not found
  for (c in vocabulary) {
    rhm <-
      glue(
        'http://ytenx.org/zim?dzih={c}&kyonh=1&jtkb=1&jtdt=1&jtkd=1&jtgt=1'
      ) %>%
      read_html() %>%
      html_node('p.yonh a') %>%
      html_attr('href')
    if (!is.na(rhm)) {
      rhm <-
        glue('http://ytenx.org{rhm}') %>%
        read_html() %>%
        html_table() %>% .[[1]]
      rhm <-
        rhm[which(rhm$'X1' == '韻攝'),2]
    } else {
      rhm <- 'X'
    }
    cat(paste0(c,' --> ', rhm, '...\n'))
    DTRhymes <- rbindlist(list(DTRhymes,
                               data.table(char = c, rhymes = rhm)))
  }

  return(DTRhymes)
}

matchRymes <- function(data, dict) {
  # This function takes a list/column of data table, and then return a list of vowels for each character, according to the second argument, a data table as a look-up table
  #
  # Args:
  #   data    A list or a column of a data table containing rhyming characters
  #   dict    A data table functioning as a look-up table
  # Returns:
  #   A list of list with vowels for each character
  # Side-effects:
  #   None
  map(data,
      function(x) {
        nextRhyme <- character()
        for (i in unlist(str_split(x, ''))) {
          nextRhyme <- c(nextRhyme, dict[char == i]$rhymes)
        }
        nextRhyme <- paste0(nextRhyme, collapse = '')
      })
}



if (!file.exists('rhymes.csv')) {
  DTRhymes <- getRhymes(DT$lastChar)
  fwrite(DTRhymes, 'rhymes.csv')
} else {
  DTRhymes <- fread(file = 'rhymes.csv')
}

DT[ , 'rhymes' := lapply(DT[,.(lastChar)],
                             matchRymes,
                             DTRhymes)]
DT[ , 'rhymes' := lapply(.SD$rhymes,
                         function(x) (
                           str_split(x, '') %>%
                             lapply(function(x) paste0(x, ' ')) %>%
                             unlist() %>%
                             reduce(str_c) %>%
                             str_remove_all(' $')))]

allEqualRhyme <- function(x) {
  #len <- str_count(x, ' ')+1
  return(length(unique(unlist(str_split(x, ' ')))) > 1)
  # if (length(stri_unique(unlist(str_split(x, ' ')))) == 1) {
  #   return(1)
  # } else {
  #   return(0)
  # }
}

getRhymes <- function(data) {
  # This function takes a list/column of data table, and then generate another data table with vowels of those characters
  #
  # Args:
  #   data    A list or a column of a data table containing rhyming characters
  # Returns:
  #   A data table consists of the vowels of those rhyming characters
  # Side-effects:
  #   None
  DTRhymes <- data.table(char = character(), rhymes = character())

  vocabulary <-
    str_split(data, '') %>%
    unlist() %>%
    unique()

  # Only take the first vowel if there're multiple available
  # Return NA if it's not found
  for (c in vocabulary) {
    rhm <-
      glue(
        'http://ytenx.org/zim?dzih={c}&kyonh=1&jtkb=1&jtdt=1&jtkd=1&jtgt=1'
      ) %>%
      read_html() %>%
      html_node('p.yonh a') %>%
      html_attr('href')
    if (!is.na(rhm)) {
      rhm <-
        glue('http://ytenx.org{rhm}') %>%
        read_html() %>%
        html_table() %>% .[[1]]
      rhm <-
        rhm[which(rhm$'X1' == '韻攝'),2]
    } else {
      rhm <- 'X'
    }
    cat(paste0(c,' --> ', rhm, '...\n'))
    DTRhymes <- rbindlist(list(DTRhymes,
                               data.table(char = c, rhymes = rhm)))
  }

  return(DTRhymes)
}

matchRymes <- function(data, dict) {
  # This function takes a list/column of data table, and then return a list of vowels for each character, according to the second argument, a data table as a look-up table
  #
  # Args:
  #   data    A list or a column of a data table containing rhyming characters
  #   dict    A data table functioning as a look-up table
  # Returns:
  #   A list of list with vowels for each character
  # Side-effects:
  #   None
  map(data,
      function(x) {
        nextRhyme <- character()
        for (i in unlist(str_split(x, ''))) {
          nextRhyme <- c(nextRhyme, dict[char == i]$rhymes)
        }
        nextRhyme <- paste0(nextRhyme, collapse = '')
      })
}



if (!file.exists('rhymes.csv')) {
  DTRhymes <- getRhymes(DT$lastChar)
  fwrite(DTRhymes, 'rhymes.csv')
} else {
  DTRhymes <- fread(file = 'rhymes.csv')
}

DT[ , 'rhymes' := lapply(DT[,.(lastChar)],
                         matchRymes,
                         DTRhymes)]
DT[ , 'rhymes' := lapply(.SD$rhymes,
                         function(x) (
                           str_split(x, '') %>%
                             lapply(function(x) paste0(x, ' ')) %>%
                             unlist() %>%
                             reduce(str_c) %>%
                             str_remove_all(' $')))]

allEqualRhyme <- function(x) {
  return(length(unique(unlist(str_split(x, ' ')))) == 1)
}

DT[ , 'correctRhyming' := unlist(lapply(.SD$rhymes, allEqualRhyme))]

