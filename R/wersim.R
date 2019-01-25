#' Introducing additional Word Error into a corpus
#'
#' This function introduces word error into a corpus. We find word error to consist of three
#' components: deletions (words that are missing in the ASR transcription),
#' insertions (words in the ASR transcription that are not in the reference text)
#' and substitutions (words with inaccurate ASR transcription). In the function, you have to specify
#' the ratio between deletions, insertions and substitutions.
#' As a baseline, for deletions D, we simply randomly draw a
#' unique token from the corpus and delete it from a randomly chosen text it occurs in, and
#' repeat this until we have reached ND number of deletions needed. In turn, for insertions I,
#' we randomly select a token from the corpus and insert it in a text, repeating this NI times.
#' Last, we create NS substitutions S by randomly selecting a unique token and replacing it
#' with the token from the corpus that has the smallest Levenshtein distance (Levenshtein,1966)
#' to the selected token, which measures the similarity between two strings on the basis
#' of single-character differences, e.g. "bad" may be replaced by "bat". The random draw of the token
#' for all three operations is weighted by the number of times the word occured in a specific text.
#' This means that more common words in longer texts are more likely to be chosen.
#' @param x A quanteda corpus to be modified
#' @param target_wer The additional error that needs to be introduced into the corpus
#' @param deletions The share of word error that should be introduced through deletions
#' @param insertions The share of word error that should be introduced through insertions
#' @param substitutions The share of word error that should be introduced through substitutions
#' @param groupingvar The variable that groups the corpus
#' @keywords word error rate wersim transcriptions
#' @return Returns a quanteda dfm of the corpus with added error as specified.
#' @export
#' @examples
#' # For an example, please see the documentation of the wersimtext function

wersim<-function(x,target_wer=0.05,deletions=0.13,insertions=0.22,substitutions=0.65,groupingvar){
  ##### Errors
  # Add an error if target WER is below 0
  if (target_wer < 0)
    stop("'target_wer' must be 0 or above")

  # Add an error if error ratio doesn't make sense
  if (any(c(deletions,insertions,substitutions) < 0 | c(deletions,insertions,substitutions) > 1))
    stop("all elements of error ratio must be between 0 and 1")

  # Add an error if error ratio doesn't add up to 1
  if (sum(c(deletions,insertions,substitutions))!=1)
    stop("all elements of error ratio must add up to 1")

  docvars(x,"id")=1:length(x$documents$texts)
  ids_save=unique(docvars(x,"id"))
  ClosestMatch2 = function(string, stringVector){

    distance = RecordLinkage::levenshteinSim(string, stringVector);
    stringVector[distance == max(distance)]

  }
  errors=round(target_wer*sum(ntoken(x)))
  num_del=round(errors*deletions)
  num_ins=round(errors*insertions)
  num_sub=round(errors*substitutions)
  words_in_corpus=unique(unlist(tokens(x)))
  dfm_save=as.matrix(dfm(x,remove_punct = TRUE,
                         remove_numbers=TRUE,groups = groupingvar))
  #####
  #Deletions
  if(num_del>0){
    how_many_del=num_del
    while(how_many_del>0){
      which.del=sample(length(dfm_save),prob = dfm_save,1)
      dfm_save[which.del]=dfm_save[which.del]-1
      how_many_del=how_many_del-1
    }
  }
  #####
  #Substitutions Internal
  if(num_sub>0){
    how_many_sub=num_sub
    while(how_many_sub>0){
      row_change=sample(dim(dfm_save)[1],prob = rowSums(dfm_save),1)
      col_change=sample(dim(dfm_save)[2],prob = colSums(dfm_save),1)
      if(dfm_save[row_change,col_change]>0){
        dfm_save[row_change,col_change]=dfm_save[row_change,col_change]-1
        word_change=colnames(dfm_save)[col_change]
        changed_word=sample(ClosestMatch2(word_change,
                                          words_in_corpus[!words_in_corpus%in%word_change]),1)
        dfm_save[row_change,colnames(dfm_save)==changed_word]=
          dfm_save[row_change,colnames(dfm_save)==changed_word]+1
        how_many_sub=how_many_sub-1
      }
    }
  }
  #####
  #Insertions Internal
  if(num_ins>0){
    how_many_ins=num_ins
    while(how_many_ins>0){
      which.ins=sample(length(dfm_save),prob = dfm_save,1)
      dfm_save[which.ins]=dfm_save[which.ins]+1
      how_many_ins=how_many_ins-1
    }
  }
  dfm_save=as.dfm(dfm_save)
  return(dfm_save)
}
