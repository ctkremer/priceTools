#' Species biomasses in communities X and Y
#'
#' A small example dataset containing the biomass and names of 30 species in two plant communities.
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{species}{list of species ids}
#'   \item{biomassX}{mass of species in community X, in grams/m^2}
#'   \item{biomassY}{mass of species in community Y, in grams/m^2}
#' }
#' @source Placeholder
"biomass"



#' Plant biomass data from Cedar Creek, MN
#'
#' Data on species' biomass from Cedar Creek experiment e001, Field D, 1992. This experiment contains
#' a nitrogen addition manipulation, with a control (no addition) and seven levels of increasing nitrogen
#' addition.
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{Plot}{Plot ID}
#'   \item{NTrt}{Nitrogen treatment code}
#'   \item{NAdd}{Amount of nitrogen added}
#'   \item{Species}{Species name}
#'   \item{Biomass}{Biomass of species in a given community, in grams/m^2}
#' }
#' @source Tilman and Downing 1997, and others.
"cedarcreek"