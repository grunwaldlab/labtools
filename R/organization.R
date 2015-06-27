#===================================================================================================
#' Add color css to table ?
#' 
#' Add color css to table ?
#' 
#' @param  table_html (\code{character} of length 1)
#' @param  columns (\code{integer})
#' @param  colors ?
color_table <- function(table_html, columns, colors) {
  split_str_by_index <- function(target, index) {
    index <- sort(index)
    substr(rep(target, length(index) + 1),
           start = c(1, index),
           stop = c(index -1, nchar(target)))
  }
  
  #Taken from https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
  interleave <- function(v1,v2)
  {
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1,v2)[order(c(ord1,ord2))]
  }
  
  insert_str <- function(target, insert, index) {
    insert <- insert[order(index)]
    index <- sort(index)
    paste(interleave(split_str_by_index(target, index), insert), collapse="")
  }
  
  #if colors is numeric, make into hex codes
  numeric_cols <- sapply(colors, is.numeric)
  colors[, numeric_cols] <- lapply(colors[, numeric_cols, drop=FALSE],
                                   plotrix::color.scale,
                                   alpha=NULL,
                                   na.color="#FFFFFF",
                                   cs1=c(.7,.7,1), cs2=c(.7,1,.7), cs3=c(1,.7,.7))
  #find locations of all <TD ..> tags
  td_locations <- gregexpr(pattern ='<TD', table_html, ignore.case = TRUE)[[1]] + 3
  #Infer number of columns from cound of header <TH ...> tags
  column_count <- length(gregexpr(pattern ='<TH', table_html, ignore.case = TRUE)[[1]])
  #structure the <TD> locations in the same way 'colors' is structured
  td_locations <- as.data.frame(matrix(td_locations, ncol=column_count, byrow=TRUE))
  td_locations <- td_locations[,columns]
  #insert inline style into the html table
  td_locations <- unlist(td_locations)
  colors <- unlist(colors)
  colors <- paste(' style="background-color:', colors, ';",', sep="")
  insert_str(table_html, colors, td_locations)
}


#===================================================================================================
#' Standardizes the printing of data.frames
#'
#' Standardizes the printing of data.frames and allows for renaming of columns without changing original data.frame.
#' @param data The data frame to be printed
#' @param column_names Acharacter vector of column names to be applied.
#' @param colors The columns of 'data' that should be colored based of their numeric content (e.g. data[, c('my_col'), drop=FALSE]).
#' Also accepted: A named list of vectors with names indicating columns of data and values indicating colors for the corresponding cells of 'data'. 
#' @param md (\code{logical} of length 1) If \code{TRUE}, return a markdown table istead of HTML.
#' @param ... Additional key word arguments are passed to \code{\link[xtable]{print.xtable}}
#' @export
print_table <- function(data, column_names=NA, colors=NA, md = FALSE, ...) {
  original_names <-  names(data)
  if (is.character(column_names)) {
    names(data) <- column_names
  }
  numeric_cols <- sapply(data, is.numeric)
  data[, numeric_cols] <- lapply(data[, numeric_cols, drop=FALSE], signif, digits=3)
  data[] <- lapply(data, as.character)
  data[is.na(data)] <- ""
  if (md) {
    print(knitr::kable(data))
  } else {
    output_table <- print(xtable::xtable(data),
                          type = "html",
                          print.results=FALSE,
                          sanitize.text.function = function(x) {x}, 
                          ...)
    if (is.list(colors)) {
      colored_cols <- which(sapply(original_names, function(x) x %in% names(colors))) + 1
      output_table <- color_table(output_table, colored_cols, colors)
    }
    x <- cat(output_table)
  }
}


#===================================================================================================
#' Find current project root
#' 
#' Finds the current project's root directory from any subdirectory by looking through the file 
#' path for a .Proj extension.
#' 
#' @param path (\code{character} of length 1) The path of a subdirectory of the project.
#' 
#' @return The path to the current project's root
#' 
#' @export
get_project_root <- function(path = getwd()) {
  if ("Rproj" %in% tools::file_ext(list.files(path))) {
    return(path)
  } else if (path %in% c("", NA, .Platform$file.sep)) {
    stop("Could not find project root in given path or current working directory.")
  } else {
    return(get_project_root(dirname(path)))
  }
}
