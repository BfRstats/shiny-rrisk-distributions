# function that creates a shiny response R6 class
get_shiny_R6_class <- function(class_name, parent_class) 
{
  R6::R6Class(
    classname = class_name,
    inherit   = parent_class,
    # internal private data
    private = list(# below for making R6 class reactive in shiny
      reactiveDep  = NULL,
      reactiveExpr = NULL,
      tickle       = TRUE,
      tickle_shiny = function() 
      {
        private$tickle <- !private$tickle
        private$reactiveDep(private$tickle)
        invisible(NULL)
      }
    ),
    public = list(
      initialize = function(...) 
      {
        # initialize inheritate R6 class
        super$initialize(...)
        # For telling shiny the R6 class has changed.
        # Until someone calls $reactive(),
        # private$reactiveDep() is a no-op.
        # Need to be set in here, because if
        # it's set in the definition of
        # private above, it will be locked
        # and can't be changed.
        private$reactiveDep <- function(x) NULL
      },
      reactive = function() 
      {
        if (is.null(private$reactiveExpr)) {
          private$reactiveDep  <- reactiveVal(logical(0))
          private$reactiveExpr <- reactive({
            private$reactiveDep()
            self
          })
        }
        private$reactiveExpr
      }
    ),
    lock_class = TRUE # disable $set(...) function
  )
}