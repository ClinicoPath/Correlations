
#' Correlation Analysis
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr "%>%"
#' @importFrom correlation correlation
#' @importFrom glue glue
#' @import psych
#' @export

# This file is a generated template, your changes will not be overwritten


# `self$data` contains the data
# `self$options` contains the options
# ..corrvars - variables to calculate correlations for
# ..ctrlvars - variables to control for
# ..shwSig  - show significance level for correlations (p-values)
# ..flgSig  - flag significant correlations
# ..sidSig  - one- or two-tailed significance calculations
# `self$results` contains the results object (to populate)


correlationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "correlationClass",
    inherit = correlationBase,
    private = list(
        # ====================================================
#        .init = function() {
            # get variables
           # matrix <- self$results$get('matrix')
            #varCrr <- self$options$get('corrvars')
 #           varCtl <- self$options$get('ctrlvars')

            # set title according to whether the procedure is controlling for variables or not
 #           matrix$setTitle(ifelse(length(varCtl) > 0, 'Partial Correlation Matrix', 'Correlation Matrix'))




  #      },

        .run = function() {
            # get variables
            #matrix <- self$results$get('matrix')
            #varCrr <- self$options$get('corrvars')
            #varCtl <- self$options$get('ctrlvars')

            # TODO

            todo <- glue::glue(
                "This Module is still under development
                - Correlations by Hyunsoo Seol
                -
                "
            )

            self$results$todo$setContent(todo)


            # Data definition--------------


            if (length(self$options$corrvars) < 2)
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


          # Correlation-----------------------


          mydata <- self$data

          formula <- jmvcore::constructFormula(terms = self$options$corrvars)

          myvars <- jmvcore::decomposeFormula(formula = formula)

          myvars <- unlist(myvars)

###-----------------------------------

          #           library(dplyr)
          #           mydata<- tibble(mtcars)
          #            mydata
          #            class(mydata)

          #           library(correlation)

          #           cor1 <- mydata %>% select(mpg, wt, drat) %>% correlation(.)

          #           cor1
###--Pearson-------------------------------------

          cor1 <- mydata %>%
            select(myvars) %>%
            correlation::correlation(.)

          self$results$text1$setContent(cor1)

###-------------------------------------------

#           corx <- mydata %>%
#               dplyr::select(myvars)  %>%
#               stats::cor.test(method = "spearman", exact = FALSE) %>%
#               report::report()

#          cor2 <- cor1 %>%
#               report::report(.)


#           self$results$text2$setContent(cor2)

###-------Spearman------------------------------------------

          cor2 <- mydata %>%
            select(myvars) %>%
            correlation::correlation(method = "spearman")

          self$results$text2$setContent(cor2)

###-----Tetrachoric--------------------------

          cor3 <- mydata %>%
            select(myvars) %>%
            correlation::correlation(method = "tetrachoric")

          self$results$text3$setContent(cor3)




        }
    )
)
