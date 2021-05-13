load("SingleWordProbs.RData")
load("EmissionMatrix.RData")
load("TransitionMatrix.RData")
load("ListOfWords.RData")


makeSonnet <- function(verbose=F){
  Write = TRUE
  State = "StateFirstProb"
  MarkQuote      <- character()            # Create a vector for storing the new sequence
  
  ########
  # Choose the word for the first position in the sequence:
  FirstMarkovWord <- sample(names(ListOfWords[[State]]), 1, rep=TRUE, prob=ListOfWords[[State]])
  MarkQuote[1]   <- FirstMarkovWord       # Store the nucleotide for the first position of the sequence
  i = 2
  # Change to next state
  State = "StateBody"
  if(verbose) {cat("\n")}
  while (Write == TRUE) {
    if(verbose) {print (State)}
    ### Choose word according to State
    PreviousWord <- MarkQuote[i-1]
    
    # Get the previous nucleotide in the new sequence
    # Get the probabilities of the new word, given previous word AND the NEW state
    PreviousWord <- gsub("*\\.[0-9]", "\\", PreviousWord)
    probabilities  <- TransitionMatrix[PreviousWord,] ## Probabilities of a word given previous word
    #
    OverlapIndex <- names(ListOfWords[[State]]) %in% names(probabilities)[probabilities != 0] #Posible words given a previous word found in current state
    words <- names(ListOfWords[[State]]) [OverlapIndex]
    probabilities  <- TransitionMatrix[PreviousWord,words,drop=F]
    probabilities
    ## Select word and save
    if (length(probabilities) != 0) { 
      NextWords     <- sample(names(probabilities), 1, rep=TRUE, prob=probabilities) #According to transition prob matrix
      NextWords <- gsub("*\\.[0-9]", "\\", NextWords)
      MarkQuote[i]  <- NextWords          # Store the nucleotide for the current position of the sequence
      # If there are no possible words given the current state and previous word, 
      #select a random from that state without considering previous word
    } else { MarkQuote[i]     <- sample(names(ListOfWords[[State]]), 1, rep=TRUE, prob=ListOfWords[[State]])
    }     
    
    # If sentence end, add period. 
    if (State == "StateSentenceEnd") { 
      MarkQuote[i] <- paste(MarkQuote[i], ". \n",sep="", collapse = "")}
    
    ###################################################################################################
    
    ### Select new state
    StateNames <- names(EmissionMatrix[State,])
    StateProbs <- EmissionMatrix[State,]
    State     <- sample(StateNames, 1, rep=TRUE, prob=StateProbs)
    
    
    ###################################################################################################
    ## Check if end sentence. 
    if (State == "STOP") { Write = FALSE}
    i=i+1
  }
  return(MarkQuote)
}


capitalizeQuote <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}