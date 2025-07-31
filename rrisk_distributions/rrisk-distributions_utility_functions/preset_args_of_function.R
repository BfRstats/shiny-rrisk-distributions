# preset arguments in a function
# e.g.: 
# f <- function(a,b) a + b
# new_f <- preset_args(f, b = 10)
# new_f(2) # will return 12
# this function replaces purrr::partial
preset_args <- function(f, ...)
{
  # init
  new_args_list <- list(...)
  # get function arguments
  func_args <- formals(f)
  # set new function arguments
  for (this_arg_name in names(new_args_list)) {
    func_args[this_arg_name] <- list(new_args_list[[this_arg_name]])
  }
  # set new function arguments to function
  formals(f) <- as.pairlist(func_args)
  # return function
  f
}