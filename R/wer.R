#' Calculating the Word Error Rate
#'
#' This function calculates the word error rate between a hypothesis and a reference corpus.
#' @param r The reference quanteda corpus
#' @param h The hypothesis quanteda corpus
#' @keywords word error rate
#' @export
#' @return A dataframe containing the Word error rate, the number of
#' substitutions, deletions and insertions and the number of words in the
#' reference and hypothesis corpora for each text in the corpus.
#' @examples
#' hypothesis_data=data.frame(text="The meadoww very nice and the two sun shines bright",
#' name="doc1",stringsAsFactors = F)
#' hypothesis_corpus=quanteda::corpus(hypothesis_data,docid_field = "name", text_field = "text")
#' reference_data=data.frame(text="The meadow is very nice and the sun shines bright",
#' name="doc1",stringsAsFactors = F)
#' reference_corpus=quanteda::corpus(reference_data,docid_field = "name", text_field = "text")
#' wer(r=reference_corpus,h=hypothesis_corpus)

wer<-function(r,h){
  ##### Errors
  # Add an error if corpora have different lengths
  if (length(r$documents$texts)!=length(h$documents$texts))
    stop("The refernce and hypothesis corpus should have the same length")

  if(length(r$documents$texts)==length(h$documents$texts)){
    data.store=data.frame(wer=rep(NA,length(r$documents$texts)),
                          sub=NA,ins=NA,del=NA,words.ref=NA,words.hyp=NA)
    for(k in 1:length(r$documents$texts)){
      print(paste("Document",k,"of",length(r$documents$texts)))
      sub.count=0
      ins.count=0
      del.count=0
      ref_text=tolower(unlist(stringr::str_split(r$documents$texts[k]," ")))
      hyp_text=tolower(unlist(stringr::str_split(h$documents$texts[k]," ")))
      if(h$documents$texts[k]==""){
        data.store$wer[k]=1
        data.store$sub[k]=0
        data.store$del[k]=length(ref_text)
        data.store$ins[k]=0
        data.store$words.ref[k]=length(ref_text)
        data.store$words.hyp[k]=0
      }
      if(h$documents$texts[k]!=""){
        d1<-matrix(ncol=length(hyp_text)+1,nrow=length(ref_text)+1,0)
        d1[1,]<-0:length(hyp_text)
        d1[,1]<-0:length(ref_text)
        dtext=d1
        for(i in 2:nrow(d1)){
          for(j in 2:ncol(d1)){
            if(ref_text[i-1]==hyp_text[j-1]){
              d1[i,j]<-d1[i-1,j-1]
              dtext[i,j]="CORRECT"
            }
            else{
              sub<-d1[i-1,j-1]+1
              ins<-d1[i,j-1]+1
              del<-d1[i-1,j]+1
              d1[i,j]<-min(sub,ins,del)
              if(which.min(c(sub,ins,del))==1){
                dtext[i,j]="SUB"
              }
              if(which.min(c(sub,ins,del))==2){
                dtext[i,j]="INS"
              }
              if(which.min(c(sub,ins,del))==3){
                dtext[i,j]="DEL"
              }
            }
          }
        }
        sequence=rep(NA,length(ref_text))
        start.row=nrow(dtext)
        start.col=ncol(dtext)
        dtext[2:nrow(dtext),1]="DEL"
        dtext[1,2:ncol(dtext)]="INS"
        dtext[1,1]="CORRECT"
        for(l in (length(sequence)):1){
          sequence[l]=dtext[start.row,start.col]
          if(sequence[l]%in%c("CORRECT","SUB")){
            start.row=start.row-1
            start.col=start.col-1
          }
          if(sequence[l]=="DEL"){
            start.row=start.row-1
          }
          if(sequence[l]=="INS"){
            start.col=start.col-1
          }
        }
        data.store$wer[k]=d1[length(ref_text)+1,length(hyp_text)+1]/length(ref_text)
        data.store$sub[k]=sum(sequence=="SUB",na.rm=T)
        data.store$del[k]=sum(sequence=="DEL",na.rm=T)
        data.store$ins[k]=sum(sequence=="INS",na.rm=T)
        data.store$words.ref[k]=length(ref_text)
        data.store$words.hyp[k]=length(hyp_text)
      }
    }
    return(data.store)
  }
}
