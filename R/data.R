#' @title Q-Sorts on taxation and the economy from the 2014 CiviCon Citizen Conference
#'
#' @description
#' A dataset of 18 participants sorting 77 items on taxation and the economy before and after the 2014 CiviCon Citizen Conference.
#'
#' @format A list with 2 elements:
#' \describe{
#'   \item{sorts}{the Q-sorts, as an array with three named dimensions:
#'     \describe{
#'       \item{items}{with item-handles as dimension names,}
#'       \item{people}{with anonymized participant names as dimension names,}
#'       \item{conditions}{with `before` and `after` as dimension names.}
#'     }
#'   }
#'   \item{set}{the Q-set of full item wordings, as a matrix with two named dimensions:
#'     \describe{
#'       \item{items}{as rows, with item-handles as rownames,}
#'       \item{languages}{`english` and `german` as column names.}
#'     }
#'   }
#' }
#' @source \url{http://www.maxheld.de/schumpermas/}
"civicon_2014"

#' @title Q-Sorts and Q-Categorisations on language games
#'
#' @description
#' A dataset of 53 participants, sorting and categorizing 35-41 items.
#' Part of Verena Kasztantowicz's dissertation at Humboldt University of Berlin, Germany.
#'
#' @format A list with 3 elements:
#' \describe{
#'   \item{items}{a dataframe with german and english handles,}
#'   \item{qsorts}{a matrix with items as rows, participants as columns and Q-sorts in cells.}
#'   \item{qcat}{Q categorisation in its canonical form.}
#' }
#' @source \url{https://www.erziehungswissenschaften.hu-berlin.de/de/institut/mitarbeiter/1688111}
"komki"
