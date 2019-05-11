initPackage <- function(lib) {
  # Load packages, and install them if not installed yet.
  #
  # Args:
  #   lib: A Character vector with names of libraries to be loaded
  #
  # Returns:
  #   None
  #
  # Side-Effects:
  #   1. Install required libraries if not installed yet
  #   2. Load the library into Global Environment
  #   3. Remove itself
  tryCatch(
    {
      for (l in lib) {
        eval(parse(text=paste0("library(",l,")")))
      }
    },
    error = function(e) {
      cat(l, 'not found...\nInstalling', l, 'now...\n')
      install.packages(l)
      initPackage(lib)
    },
    finally = cat('All packages loaded!')
  )

  on.exit(rm(initPackage, envir = .GlobalEnv))
}

initPhantomJS <- function() {
  # Download PhantomJS if it's not there. Currently only supports Linux and macOS and there's no plan to support non-*nix system
  #
  # Args:
  #   None
  # Returns:
  #   None
  # Side-effects:
  #   1. A file named phantomjs is stored in the working directory.
  #   2. Remove itself
  if (!file.exists('phantomjs')) {
    if (Sys.info()['sysname'] == 'Linux') {
      phJS_url <- 'https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-2.1.1-linux-x86_64.tar.bz2'
      phJS_name <- 'phatomjs.tar.bz2'
      phJS_bin <- './phantomjs-2.1.1-linux-x86_64/bin/phantomjs'
      phJS_dir <- './phantomjs-2.1.1-linux-x86_64'
    } else if (Sys.info()['sysname'] == 'Darwin') {
      phJS_url <- 'https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-2.1.1-macosx.zip'
      phJS_name <- 'phatomjs.zip'
      phJS_bin <- './phantomjs-2.1.1-linux-macos/bin/phantomjs'
      phJS_dir <- './phantomjs-2.1.1-linux-macos'
    }
    download.file(phJS_url, phJS_name)
    untar(phJS_name)
    file.copy(phJS_bin, getwd())
    file.remove(phJS_name)
    unlink(phJS_dir, TRUE)
  }

  on.exit(rm(initPhantomJS, envir = .GlobalEnv))
}

dlMenu <- function() {
  # Download the meta-data containing the url to each individual peoms
  #
  # Args:
  #   None
  # Returns:
  #   A data.table with url to the following categories of Tang poems:
  #     1. Wulü, 5-character 8-line verse
  #     2. Qilü, 7-character 8-line verse
  #     3. Wujue, 5-character 4-line verse
  #     4. Qijue, 7-character 4-line verse
  # Side-effects:
  #   Remove it self
  DTMenu <- data.table(type = character(),
                       author = character(),
                       title = character(),
                       url = character())
  typeNames <- c('Wulü', 'Qilü', 'Wujue', 'Qijue')

  for (i in 1:4) {
    batch <-
      read_html('https://zh.wikisource.org/wiki/唐詩三百首') %>%
      html_nodes(xpath = glue('/html/body/div[3]/div[3]/div[4]/div/ol[',
                              i + 3,
                              ']/li'))
    type <- typeNames[i]
    author <-
      batch %>%
      html_text() %>%
      str_extract('.+?(?= )')
    title <-
      batch %>%
      html_nodes('a') %>%
      html_attr('title')
    url <-
      batch %>%
      html_nodes('a') %>%
      html_attr('href') %>%
      map(function(x) glue('https://zh.wikisource.org', x)) %>%
      unlist()
    DTMenu <-
      rbindlist(
        list(DTMenu,
             data.table(type = type,
                        author = author,
                        title = title,
                        url = url)))
  }

  on.exit(rm(dlMenu, envir = .GlobalEnv))
  return(DTMenu)
}

dlPoems <- function (menu) {
  # Dowload the Poems and store them as a html file, if any of them are not downloaded yet
  #
  # Args:
  #   menu    A data.table with url and title for downloading
  # Returns:
  #   None
  # Side-effects:
  #   1. Download all poems under WORKING_DIR/poems
  #   2. Remove itself
  if (!dir.exists('poems')) {
    dir.create('poems')
  }
  if (!file.exists('scrape.js')) {
    stop("scrape.js doesn't exist!")
  }

  # Maximum number of acts in menu
  apply(
    menu,
    MARGIN = 1,
    function(ro) {
      author <- eval(parse(text=glue("ro['author']")))
      title <- eval(parse(text=glue("ro['title']")))
      url <- eval(parse(text=glue("ro['url']")))
      dest <- glue('./poems/{file}', file = glue('{title}-{author}.html'))
      if (!file.exists(dest)) {
        cat(sprintf('Downloading %s from %s ...\n', title, url))
        execute <- glue('./phantomjs scrape.js "{url}" "{dest}"')
        system(execute)
      }
    }
  )

  cat('All files are downloaded under "poems" dir\n')
  on.exit(rm(dlPoems, envir = .GlobalEnv))
}
