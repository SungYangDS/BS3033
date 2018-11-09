#' @title Pairwise Alignment
#'
#' @description A pairwise alignment function
#'
#' @param str1,str2, m,s,g
#'
#' @return scores
#'
#' @examples gen_matrix(str1,str2,m=2,s=2,g=2)
#'
#' @export gen_matrix

gen_matrix= function(str1, str2, m, s, g){
  sq1= unlist(strsplit(str1, split= ""))
  sq2= unlist(strsplit(str2, split= ""))

  MX = matrix (0, nrow = (length(sq2)+1), ncol = (length(sq1)+1))
  first_row= seq(1, length(sq1), by= 1)* g
  first_column= seq(1, length(sq2), by= 1)* g

  MX[1,c(2:(length(sq1)+1))] = first_row
  MX[c(2:(length(sq2)+1)),1] = first_column

  for (i in 2:(length(sq1)+1)){
    for (j in 2:(length(sq2)+1)){
      if (sq1[(i-1)] == sq2[(j-1)]) {diagonal= MX[(j-1), (i-1)] + m}
      else {diagonal= MX[(j-1), (i-1)] + s}

      above= MX[(j-1), i] + g
      left= MX[j, (i-1)] + g

      outcomes= c(left, above,diagonal)

      MX[j,i]= max(outcomes)
    }
  }

  alignment_1= c()
  alignment_2= c()

  i= length(sq1)+1
  j= length(sq2)+1

  while (i > 1 && j > 1) {
    currentscore= MX[j, i]
    diagonalscore= MX[(j-1), (i-1)]
    abovescore= MX[(j-1), i]
    leftscore= MX[j, (i-1)]
    outcomes= c(diagonalscore, abovescore, leftscore)

    if (outcomes[1] == max(outcomes)){
      alignment_1= append(alignment_1, sq1[i-1])
      alignment_2= append(alignment_2, sq2[j-1])
      j= j-1
      i= i-1
    }

    else if (outcomes[2]== max(outcomes)){
      alignment_1= append(alignment_1, sq1[i-1])
      alignment_2= append(alignment_2, sq2[j-1])
      j= j-1
    }

    else if (outcomes[3]== max(outcomes)){
      alignment_1= append(alignment_1, sq1[i-1])
      alignment_2= append(alignment_2, "-")
      i= i-1
    }
  }

  a1= paste(rev(alignment_1), collapse = "")
  a2= paste(rev(alignment_2), collapse = "")

  if (length(alignment_1)== length(alignment_2)){
    matching= alignment_1==alignment_2
    numbr_of_sim= sum(matching== TRUE)
  }
  print (numbr_of_sim)
}
