#' @title Hamming Distance
#'
#' @description Hamming Distance modified to for biology for aligning biological sequences. However, this is still a prototype.
#'
#' @param s1,s2,gapO,gapE
#'
#' @return total_score
#'
#' @examples hamming_distance(s1,s2,2,2)
#'
#' @export

hamming_distance<- function(s1,s2,gapO,gapE){
  s1<-unlist(strsplit(s1,""))
  s2<-unlist(strsplit(s2,""))
  bin= 0
  Total_score =0

  if (length(s1) == length(s2)){
    matching= s1==s2
    matchg= s1==s2
    sim= sum(matchg== TRUE)
    gap= sum(matchg== FALSE)
    matchg= paste(as.numeric(matchg), collapse= "")
    if (grepl("10", matchg)){
      bin= bin + 1
    }
    Total_score= Total_score + sim - bin*gapO - (gap-bin)*gapE
  }
  else
  {
    x1= s1[1:min(c(length(s1), length(s2)))]
    x2= s2[1:min(c(length(s1), length(s2)))]
    matchg= x1==x2
    sim= sum(matchg== TRUE)
    gap= sum(matchg== FALSE)
    matchg= paste(as.numeric(matchg), collapse= "")
    if (grepl("10", matchg)){
      bin= bin + 1
    }
    Total_score= Total_score + sim - bin*gapO - (gap-bin)*gapE
  }
  print(Total_score)
}
