#' Run Text models on corpora that were modified using the WERSIM function
#'
#' This function calculates the word error rate between a hypothesis and a reference corpus.
#' @param x A quanteda corpus to be modified
#' @param measured_wer The word error rate (or an estimate thereof) of corpus x
#' @param new_wer The word error rate for which the text model should be run. Generally, this is measured_wer plus some fixed incremental error such as 0.05
#' @param deletions_sim The share of word error that should be introduced through deletions
#' @param insertions_sim The share of word error that should be introduced through insertions
#' @param substitutions_sim The share of word error that should be introduced through substitutions
#' @param num_sims The number of simulations to be run
#' @param preprocessing The preprocessing that should be done with the corpus. Defaults to c("punctuation","numbers") for excluding numbers and punctuation, can take on "min_term", stemming", "stopwords_en", "stopwords_de".
#' @param mincount_wersim If "min_term" is part of "preprocessing", this parameters specifices the minimum number of times a word has to be in the corpus to be retained in the dfm
#' @param method The text model that should be run on the simulated corpus. Can either be "sentiment" or "wordfish"
#' @param groupingvar_sim The variable that groups the corpus
#' @param direction For Wordfish: The parameters that are forwarded to the dir command in Wordfish (fixing the direction of the space). Defaults to c(1,2).
#' @keywords word error rate wersim transcriptions
#' @export
#' @return A data frame with the grouping variable in column 1 and simulated quantities (sentiment or Wordfish estimates) in subsequent columns.
#' @examples
#' wersimtext()

wersimtext=function(x,measured_wer,new_wer,deletions_sim=0.13,insertions_sim=0.22,substitutions_sim=0.65,num_sims,preprocessing=c("punctuation","numbers"),mincount_wersim=0,method,groupingvar_sim,direction=c(1,2)){
  how_much_wer_to_add=new_wer-measured_wer
  docvars(x,"id")=1:length(x$documents$texts)
  docvars(x,"groupingvar")=groupingvar_sim
  data_store=data.frame(index=row.names(dfm(x,groups=groupingvar_sim)))
  for(j in 1:num_sims){
    cat(paste0("Simulation Number: ",j," of ",num_sims," for ",new_wer,"\n"))
    x2=wersim(x,target_wer = how_much_wer_to_add,deletions=deletions_sim,insertions=insertions_sim,substitutions=substitutions_sim,groupingvar=groupingvar_sim)
    if("punctuation"%in%preprocessing){
      x2=dfm_trim(x2,remove_punct = TRUE)
    }
    if("numbers"%in%preprocessing){
      x2=dfm_trim(x2,remove_numbers = TRUE)
    }
    if("min_term"%in%preprocessing){
      x2=dfm_trim(x2,min_termfreq = mincount_wersim)
    }
    if("stemming"%in%preprocessing){
      x2=dfm(x2,stem=T)
    }
    if("stopwords_en"%in%preprocessing){
      x2=dfm(x2,remove=stopwords("english"))
    }
    if("stopwords_de"%in%preprocessing){
      x2=dfm(x2,remove=stopwords("german"))
    }
    if(method=="sentiment"){
      sentistore=dfm(x2, dictionary = data_dictionary_LSD2015)%>%
        convert(to="data.frame")
      sentistore$sentiment=log((sentistore$positive+0.5)/(sentistore$negative+0.5))
      names(sentistore)[names(sentistore)=="sentiment"]=paste0("sentiment",j)
      data_store=cbind(data_store,sentistore[,grepl(names(sentistore),pattern=j)])
    }
    if(method=="wordfish"){
      wordfishstore=austin::wordfish(quanteda::as.wfm(x2),dir=direction)
      wordfishstore=data.frame("theta"=wordfishstore$theta)
      names(wordfishstore)[names(wordfishstore)=="theta"]=paste0("theta",j)
      data_store=cbind(data_store,wordfishstore[,grepl(names(wordfishstore),pattern=j)])
    }
  }
  return(data_store)
}
