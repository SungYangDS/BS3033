#' @title DNA Generator
#'
#' @description Generates randomized DNA sequences
#'
#' @param n, control, t
#'
#' @return DNAStringGen[i]
#'
#' @examples DNA(4,control=1,2)
#'
#' @export DNA


DNA <- function(n, control, t) {

  #Error-returning function if t < 2
  ErrorCheck <- function() {
    StringRep(list(DNA1))
    stop("Generation of only 1 DNA: ", DNA1)
  }


  #Converts output of DNAlist to strings in forms of multiple lists (DNAStringGen)
  StringRep <- function(...){
    inflist <- lapply(..., paste, collapse = "")
    assign("DNAStringGen",inflist, envir = .GlobalEnv)
  }

  # Bins for the probablistic selection of each element for the generation of differences.
  BP1 <- c("A", "T", "C", "G")
  BP2 <- c("A", "T", "C", "G", "")

  # Probablity of each elements within the aforementioned bins. Subjected to user manipulation if necessary.
  cgat = c(0.5, 0.5, 0.5, 0.5)
  cgat2 = c(0.5, 0.5, 0.5, 0.5, 0.5)
  cgat3 = c(0.5, 0.5, 0.5)


  # Checks if control has an argument. If control is not specified, error will be returned with customised messages to notify user of the
  # problems.
  if(hasArg(control) == T & control == 2) {
    print("Positive Control to be selected.")
    DNA1 <- sample(BP1, n, replace = T)
    DNAlist <- list(DNA1)
    if (t >= 2) {
      for (i in 2:t) {
        rdm_number <- sample(seq(n), sample(1:n, 1))
        rep <- sample(BP2, length(rdm_number), replace = T, prob = cgat2)
        DNAlist <- append(DNAlist, list(replace(DNA1, rdm_number, rep)))
      }
      StringRep(DNAlist)
    } else {
      ErrorCheck()
    }
  } else if (hasArg(control) == T & control == 1){
    print("100% Similarity Positive Control to be selected.")
    DNA1 <- sample(BP1, n, replace = T)
    DNAlist <- list(DNA1)
    if (t >= 2) {
      for (i in 2:t) {
        DNAlist <- append(DNAlist, list(DNA1))
      }
      StringRep(DNAlist)
    }
  } else if (hasArg(control) == T & control == 0){
    print("0% Similarity Negative Control to be selected.")
    DNA1 <- sample(BP1, n, replace = T)
    DNAlist <- list(DNA1)
    rep <- DNA1
    if (t >= 2) {
      for (i in 2:t) {
        for (b in 1:length(DNA1)){
          BP3 = BP1[BP1!=(DNA1[b])]
          rep <- replace(rep, b, sample(BP3, 1))
          rep2 <- rep[1:sample(1:length(DNA1), 1)]
        }
        DNAlist <- append(DNAlist, list(rep2))
      }
      StringRep(DNAlist)
    } else {
      ErrorCheck()
    }
  } else {
    stop("No control indicated!")
  }
}
