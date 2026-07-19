#' XlsxFileComparator.R
#'
#' Specialised comparator for Excel file comparison.
#' This comparator contains the custom handling for reading Excel file
#' contents for comparison.
#'
#' @include TxtFileComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::create_comparator that will automatically create
#' # the correct comparator instance based on the file types.
#'
#' file1 <- 'my_file1.xlsx'
#' file2 <- 'my_file2.xlsx'
#' comparator <- verifyr2::create_comparator(file1, file2)
#'
#' # If needed, an explicit comparator can be created as well.
#'
#' file1 <- 'my_file1.xlsx'
#' file2 <- 'my_file2.xlsx'
#' comparator <- XlsxFileComparator$new(file1, file2)
#'
#' @export
#'
XlsxFileComparator <- R6::R6Class(
  "XlsxFileComparator",
  inherit = TxtFileComparator,
  public = list(

    #' @description
    #' Method for getting the single file contents for the comparison. The
    #' method returns the file contents in two separate vectors inside a list.
    #' The first vector is the file contents and the second one is the file
    #' contents with the rows matching the omit string excluded. This method
    #' is intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' For XlsxFileComparator, each sheet is read and every row is flattened to
    #' a single line. Each line is annotated with the sheet name and, when a
    #' header row is present, each cell is written as \code{Column=Value} so
    #' that any detected difference clearly indicates the sheet, the row and
    #' the column it originates from. Whether the first row is treated as a
    #' header is detected automatically (see the private \code{has_header_row}
    #' helper); sheets without a header row fall back to positional column
    #' names and are flagged in the sheet marker.
    #'
    #' @param file   file for which to get the contents
    #' @param config configuration values
    #' @param omit   string pattern to omit from the comparison
    #'
    vrf_contents = function(file, config, omit) {
      self$vrf_open_debug("Xlsx::vrf_contents", config)

      if ("no" == super$vrf_option_value(config, "xlsx.details")) {
        result <- super$vrf_contents(file, config, omit)

        self$vrf_close_debug()
        return(result)
      }

      sheets   <- readxl::excel_sheets(file)
      contents <- character(0)

      for (sheet in sheets) {
        # Read the sheet as-is (no assumed header) so the header row can be
        # detected explicitly.
        raw <- readxl::read_excel(
          file,
          sheet     = sheet,
          col_names = FALSE,
          col_types = "text"
        )

        # header handling can be forced via config; "auto" uses the heuristic
        header_opt <- super$vrf_option_value(config, "xlsx.header")
        has_header <- switch(
          header_opt,
          "yes" = TRUE,
          "no"  = FALSE,
          private$has_header_row(raw)
        )

        if (has_header && nrow(raw) > 0) {
          headers <- as.character(unlist(raw[1, ]))
          empty   <- is.na(headers) | headers == ""
          headers[empty] <- paste0("Column", which(empty))
          body <- raw[-1, , drop = FALSE]
        } else {
          headers <- if (ncol(raw) > 0) {
            paste0("Column", seq_len(ncol(raw)))
          } else {
            character(0)
          }
          body <- raw
        }

        # Sheet marker, flagging when no header row was detected.
        marker <- if (has_header) {
          paste0("[Sheet: ", sheet, "]")
        } else {
          paste0("[Sheet: ", sheet, " (no header row detected)]")
        }
        contents <- c(contents, marker)

        if (nrow(body) > 0) {
          prefix <- paste0("[", sheet, "] ")

          rows <- vapply(seq_len(nrow(body)), function(i) {
            values <- as.character(unlist(body[i, ]))
            values[is.na(values)] <- ""
            # Column=Value keeps the column name next to each cell so a
            # changed cell shows which column it belongs to.
            cells <- paste0(headers, "=", values)
            paste0(prefix, "row ", i, "\t", paste(cells, collapse = "\t"))
          }, character(1))

          contents <- c(contents, rows)
        }
      }

      result <- self$vrf_contents_inner(contents, config, omit)

      self$vrf_close_debug()
      result
    },

    #' @description
    #' Inherited method for indicating whether detailed comparison is available
    #' with the current comparator. Returns an empty string if the comparator
    #' is supported, otherwise a string that will be concatenated with the
    #' summary string.
    #'
    #' @param config configuration values
    #'
    vrf_details_supported = function(config) {
      if ("no" == super$vrf_option_value(config, "xlsx.details")) {
        return("Xlsx details comparison disabled.")
      }
      super$vrf_details_supported(config)
    }
  ),
  private = list(

    # Heuristic for deciding whether the first row of a sheet is a header row.
    #
    # The check is intentionally conservative to avoid mislabelling real data:
    # a header is assumed (matching the usual Excel convention and readxl's
    # default) UNLESS the first row looks like data, i.e. every non-empty cell
    # in the first row parses as a number. In that case the sheet is treated as
    # having no header row.
    has_header_row = function(raw) {
      if (is.null(raw) || nrow(raw) < 1 || ncol(raw) < 1) {
        return(TRUE)
      }

      header    <- as.character(unlist(raw[1, ]))
      non_empty <- header[!is.na(header) & header != ""]

      # An empty first row cannot be a header.
      if (length(non_empty) == 0) {
        return(FALSE)
      }

      # If every non-empty first-row cell is numeric, the row looks like data
      # rather than a header.
      all_numeric <- all(!is.na(suppressWarnings(as.numeric(non_empty))))
      !all_numeric
    }
  )
)
