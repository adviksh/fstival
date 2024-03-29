#' Read fst files
#'
#' @description
#' Reads data frames from fast-storage ('fst') file,
#' after checking if the requested file and columns exist before attempting to read.
#'
#' @param path path to fst file
#' @param columns Column names to read The default is to read all columns.
#' @param from Read data starting from this row number
#' @param to Read data up until this row number. The default is to read to the last row of the stored dataset.
#' @param as.data.table If TRUE, the result will be returned as a data.table object. Any keys set on dataset x before writing will be retained. This allows for storage of sorted datasets. This option requires data.table package to be installed.
#' @param old_format must be FALSE, the old fst file format is deprecated and can only be read and converted with fst package versions 0.8.0 to 0.8.10.
#'
#' @returns A data frame (or data.table) with the selected columns and rows.
#'
read_fst = function(path,
                    columns = NULL,
                    from = 1,
                    to   = NULL,
                    as.data.table = FALSE,
                    old_format    = FALSE) {

  # check if file exists
  if (file.exists(path) == FALSE) { stop("File does not exist: ", path) }

  # check if any requested 'columns' are not present in the 'path'
  file_cols = metadata_fst(path)$columnNames
  miss_cols = setdiff(columns, file_cols)

  if (length(miss_cols) > 0) {
    stop("The parameter 'columns' should name columns in the file stored at 'path'.",
         "These columns are not in that file: ",
         paste(miss_cols, collapse = ", "))
  }

  fst::read_fst(path = path,
                columns = columns,
                from = from,
                to   = to,
                as.data.table = as.data.table,
                old_format    = old_format)
}


#' Read metadata from a fst file
#'
#' @description
#' Method for checking basic properties of the dataset stored in path,
#' after checking if the requested file exists.
#'
#' @inheritParams read_fst
#'
#' @returns Returns a list with meta information on the stored dataset in path. Has class fstmetadata.
#'
metadata_fst = function(path, old_format = FALSE) {

  # check if file exists
  if (file.exists(path) == FALSE) { stop("File does not exist: ", path) }

  fst::metadata_fst(path = path, old_format = old_format)
}

#' Write fst files
#'
#' @description
#' writes data frames into fast-storage ('fst') files.
#'
#' @param x a data frame to write to disk
#' @param path path to fst file
#' @param compress value in the range 0 to 100, indicating the amount of compression to use. Lower values mean larger file sizes. The default compression is set to 50.
#' @param uniform_encoding If 'TRUE', all character vectors will be assumed to have elements with equal encoding. The encoding (latin1, UTF8 or native) of the first non-NA element will used as encoding for the whole column. This will be a correct assumption for most use cases. If 'uniform.encoding' is set to 'FALSE', no such assumption will be made and all elements will be converted to the same encoding. The latter is a relatively expensive operation and will reduce write performance for character columns.
#'
write_fst = function(x, path, compress = 50, uniform_encoding = TRUE) {

  if (dir.exists(dirname(path)) == FALSE) {
    stop("Can't write file to provided path. Directory does not exist: ",
         dirname(path))
  }

fst::write_fst(x = x,
               path     = path,
               compress = compress,
               uniform_encoding = uniform_encoding)
}
