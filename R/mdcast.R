

# using ?reshape2:::cast illegally
cast <- reshape2:::cast


#' @title Cast a Molten \link[base]{data.frame} with *Multiple* `value.var`
#' 
#' @description 
#' Cast a molten \link[base]{data.frame} with *multiple* `value.var`.
#' 
#' @param data a molten \link[base]{data.frame}, 
#' returned object of \link[reshape2]{melt.data.frame}
#' 
#' @param formula casting \link[stats]{formula}, see function \link[reshape2]{dcast}
#' 
#' @param value.var \link[base]{character} \link[base]{vector}, 
#' names of columns which store the values 
#' 
#' @param ... additional parameters of functions \link[reshape2]{acast} and \link[reshape2]{dcast},
#' which eventually get passed into function `reshape2:::cast`.
#' 
#' @details
#' Function [mdcast()] is an extension of \link[reshape2]{dcast} in the following aspects,
#' 
#' \itemize{
#' 
#' \item {[mdcast] handles **m**ultiple `value.var`.
#' For the \eqn{i}-th value variable in `value.var`,
#' the \link[reshape2]{acast} columns are named in the fashion of 
#' `value[i].variable[j]`, \eqn{j=1,\cdots,J}.
#' This is follows naturally from the way \link[base]{data.frame} handles 
#' multiple \link[base]{matrix} input in `...`.
#' }
#' 
#' \item {For the \eqn{i}-th value variable in `value.var`, 
#' if one-and-only-one of the \link[reshape2]{acast} columns 
#' contains non-\link[base]{missing} elements,
#' this column is named as `value[i]`, instead of `value[i].variable[j]`.
#' Specifically, we remove the all-`NA` columns 
#' from the \link[reshape2]{acast} columns of the \eqn{i}-th value variable.
#' If one-and-only-one column remains, 
#' we convert this single-column \link[base]{matrix} into a \link[base]{vector}. 
#' We pass this \link[base]{vector} into \link[base]{data.frame} 
#' with the other \link[reshape2]{acast} columns of the other value variables.
#' This is a super useful feature in practice, 
#' e.g., some measurement only pertains to one of the multiple visits,
#' therefore we do not need to specify to which visit it corresponds.
#' }
#' 
#' }
#' 
#' @note
#' Function [mdcast()] uses unexported function `reshape2:::cast` illegally.  
#' I have asked Hadley, but he has no plan to export `reshape2:::cast`.
#' 
#' @returns 
#' Function [mdcast()] returns a \link[base]{data.frame}.
#' 
#' @examples 
#' library(reshape2)
#' head(aqm <- melt(airquality, id = c('Month', 'Day'), na.rm = TRUE, value.name = 'v1'))
#' aqm$v2 = rnorm(dim(aqm)[1L])
#' head(aqm)
#' aqm |> 
#'  dcast(formula = Month + Day ~ variable, value.var = 'v1') |> 
#'  head()
#' aqm |> 
#'  mdcast(formula = Month + Day ~ variable, value.var = c('v1', 'v2')) |>
#'  head()
#' 
#' (x <- data.frame(
#'   subj = rep(1:2, each = 3),
#'   event = rep(paste0('evt', 1:3), times = 2), 
#'   date = as.Date(c(14001:14003, 18001:18003)),
#'   y1 = rnorm(6), 
#'   y2 = c(rnorm(1), NA, NA, rnorm(1), NA, NA),
#'   y3 = c(rnorm(2), NA, rnorm(2), NA)))
#' x |> 
#'  mdcast(formula = subj ~ event, value.var = c('y1', 'y2')) # very useful !!!
#' x |> 
#'  mdcast(formula = subj ~ event) # very useful !!!
#' 
#' @keywords internal
#' @importFrom reshape2 acast
#' @importFrom stats setNames
#' @export
mdcast <- function(
    data, 
    formula, 
    ..., 
    value.var = setdiff(names(data), y = all.vars(formula))
) {
  
  if (!(nv <- length(value.var))) stop('value.var degenerate?')
  if (!is.character(value.var) || anyNA(value.var) || !all(nzchar(value.var))) stop('`value.var` must be character')
  if (nv == 1L) .Defunct(msg = 'use reshape2::dcast directly')
  
  # using ?reshape2:::cast illegally
  labels <- cast(data = data, formula = formula, value.var = value.var[1L], ...)$labels[[1L]] # see ?reshape2::dcast
  
  ret_acast <- value.var |>
    setNames(nm = _) |>
    lapply(FUN = \(v) {
      acast(data = data, formula = formula, value.var = v, ...) |> 
        cleanup_acast()
    })
  
  data.frame(
    labels, 
    ret_acast,  # 'matrix' will be turned into 'data.frame'
    check.names = FALSE
  )
  
}





#' @importFrom stats na.omit
cleanup_acast <- function(x) {
  
  # `x` is returned object from reshape2::acast
  
  z <- x[, colSums(!is.na(x)) > 0, drop = FALSE] # remove all-NA columns
  # cannot use `drop = TRUE` here!  If `x` is 'factor' 'matrix', `drop = TRUE` will remove attr(,'dim') 
  
  if (ncol(z) == 1L) return(c(z))
  # passing ncol-1 'matrix' to \link[base]{data.frame}, has undesired result in returned column names!
  
  if (!is.matrix(z)) stop('should not happen') # was: return(z)
  
  # if all-equal-but-NA, return single-column
  seq_r <- z |>
    nrow() |>
    seq_len()
  nz <- seq_r |>
    vapply(FUN = \(i) {
      z[i, ] |>
        unique() |>
        na.omit() |>
        length()
    }, FUN.VALUE = NA_integer_)
  if (all(nz %in% c(0, 1))) {
    ret <- seq_r |>
      lapply(FUN = \(i) {
        tmp <- z[i, ] |>
          unique() |>
          na.omit()
        if (!length(tmp)) return(NA)
        return(tmp)
      }) |>
      unlist()
    return(ret)
  }
  
  z1 <- as.data.frame.matrix(z)
  
  if (is.factor(z)) {
    # ?base::data.frame does not handle 'factor' 'matrix' well
    z1[] <- lapply(z1, FUN = factor, levels = levels(z))
  } 
  
  if (inherits(z, what = 'Date')) {
    # ?base::data.frame does not handle 'Date' 'matrix' well
    z1[] <- lapply(z1, FUN = .Date)
  }
  
  return(z1)
  
}


