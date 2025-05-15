
#' @title \link[reshape2]{melt} by Multiple Groups of Measurement Variables
#' 
#' @description
#' 
#' \link[reshape2]{melt} by multiple groups of measurement variables.
#' 
#' @param data a \link[base]{data.frame}
#' 
#' @param id.vars \link[base]{character} \link[base]{vector}, see function \link[reshape2]{melt.data.frame}.
#' Default is all variables not matched by `measure_rx`.
#' 
#' @param measure_rx *named* \link[base]{character} \link[base]{vector},
#' \link[base]{regex}s to determine `measure.vars` of function \link[reshape2]{melt.data.frame},
#' while `names(measure_rx)` serve 
#' as `value.name` of function \link[reshape2]{melt.data.frame}
#' 
#' @param variable.name \link[base]{character} scalar, 
#' see function \link[reshape2]{melt.data.frame}
#' 
#' @details 
#' Function [mmelt()] melts by multiple groups of measurement variables.
#' 
#' @returns 
#' Function [mmelt()] returns a \link[base]{data.frame}
#' 
#' @examples 
#' (iris0 = iris[c(1:3, 51:53, 101:103),])
#' rx = c(len = '\\.Length$', wd = '\\.Width$')
#' mmelt(iris0, measure_rx = rx, variable.name = 'Part')
#' 
#' head(iris1 <- iris0[2:5])
#' tryCatch(mmelt(iris1, measure_rx = rx, variable.name = 'Part'), error = identity)
#' iris1$Sepal.Length = NA # does not have to be NA_real_
#' mmelt(iris1, measure_rx = rx, variable.name = 'Part')
#' @importFrom cli col_magenta col_green
#' @importFrom reshape2 melt
#' @export
mmelt <- function(data, id.vars, measure_rx, variable.name = 'variable') {
  
  if (!is.data.frame(data)) stop('input must be `data.frame`')
  nm <- names(data)
  
  val_nm <- names(measure_rx)
  if (!length(val_nm) || anyNA(val_nm) || !all(nzchar(val_nm))) stop('`measure_rx` must be named')
  
  m_vars_ <- lapply(measure_rx, FUN = grep, x = nm, value = TRUE)
  # stopifnot(is.list(m_vars_))
  tmp_ <- mapply(FUN = gsub, pattern = measure_rx, x = m_vars_, MoreArgs = list(replacement = ''), SIMPLIFY = FALSE)
  o <- lapply(tmp_, FUN = order)
  tmp <- mapply(FUN = `[`, tmp_, o, SIMPLIFY = FALSE)
  m_vars <- mapply(FUN = `[`, m_vars_, o, SIMPLIFY = FALSE)
  
  if (!all(duplicated.default(tmp)[-1L])) {
    all_ <- unique.default(unlist(tmp))
    add_ <- lapply(tmp, FUN = setdiff, x = all_)
    add_ <- add_[lengths(add_) > 0L]
    lapply(seq_along(add_), FUN = \(i) { # (i = 1L)
      message(
        'Add ', 
        add_[[i]] |> paste(collapse = ', ') |> col_green(),
        ' for pattern ',
        measure_rx[[names(add_)[i]]] |> col_magenta()
      )
    })
    stop('`m_vars` (after removing `measure_rx`) not all identical')
  }
  
  if (missing(id.vars)) id.vars <- setdiff(nm, unlist(m_vars))
  if (!is.character(id.vars) || anyNA(id.vars) || !all(id.vars %in% nm)) stop('illegal `id.vars`')
  
  if (!is.character(variable.name) || length(variable.name) != 1L || any(variable.name == nm)) stop('illegal `variable.name`')

  melt_ <- function(variable.name, measure_rx, ...) {
    ans <- melt(..., variable.name = variable.name, na.rm = FALSE) # `na.rm=FALSE` is necessary!
    if (!is.factor(vval <- ans[[variable.name]])) stop('reshape2 package change?')
    levels(ans[[variable.name]]) <- gsub(pattern = measure_rx, replacement = '', x = levels(vval))
    return(ans)
  }
  
  rets <- .mapply(FUN = melt_, dots = list(
    measure.vars = m_vars, value.name = val_nm, measure_rx = measure_rx
  ), MoreArgs = list(
    data = data, id.vars = id.vars, variable.name = variable.name
  ))
  
  vshare <- c(id.vars, variable.name)
  rets0 <- lapply(rets, FUN = `[`, vshare)
  if (!all(duplicated(rets0)[-1L])) stop('should not happen')
  rets1 <- lapply(rets, FUN = \(i) { i[vshare] <- NULL; return(i) })
  return(data.frame(rets0[[1L]], rets1, check.names = FALSE))
  
}

