
#' Correlation Analysis
#'


#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr "%>%"
#' @name %>%
#' @rdname pipe
#' @export

# This file is a generated template, your changes will not be overwritten

correlationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "correlationClass",
    inherit = correlationBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

          # Data definition--------------


          if (length(self$options$vars) < 2)
            return()

          if (nrow(self$data) == 0)
            stop('Data contains no (complete) rows')

          # Correlation-----------------------


          mydata <- self$data

          formula <- jmvcore::constructFormula(terms = self$options$vars)

          myvars <- jmvcore::decomposeFormula(formula = formula)

          myvars <- unlist(myvars)


          cor1 <- mydata %>%
            select(myvars) %>%
            correlation::correlation(.)

          self$results$text1$setContent(cor1)




        })
)
