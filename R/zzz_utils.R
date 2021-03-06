# help add class tag in front of others, if not already
.append_class <- function(x, newclass){
  classes <- class(x)
  if (!(newclass %in% classes) && newclass != classes[1])
    class(x) <- c(newclass, classes)
  x
}

.msg_info    <- cli::cli_alert_info

# standardizes message when no method
not_defined <- function(fun=""){
  paste0(fun, ": no method defined on this class") %>%
    .msg_info()
}
