##### FallaciesMC1 #####
FallaciesMC1= function(
title = "FallaciesMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "While watching a debate online, you listen as one of the debaters claims, '",
quest.txt2 = "' This person has just... ", # the above 2 question texts are static text for the full question
listen1 = "There is no hard evidence that she stole the money, thus we must conclude that she did not steal the money.",
listen2 = "Since there is no proof that this drug is unsafe, we must conclude that it is safe.",
listen3 = "We cannot show for certain God exists, so we must conclude that God does not exist.",
listen4 = "My cousin said this was the best golf club on the market, so it must be the best club on the market.",
listen5 = "I read that homelessness cannot be solved, so it's insoluble.",
listen6 = "I just read about a taste test where 90% of participants preferred Coke to Pepsi, so America loves Coke more than Pepsi.",
listen7 = "You can always find firefighters at fires, so firefighters must cause fires.",
listen8 = "Old single people are always bitter, so being single makes them bitter.",
listen9 = "Adults are often more conservative than youths, so conservative values are produced by aging.",  # the above 9 listen texts are options of debate texts specific for the question
dat.size , # This is the number of values to be randomly generated for the dataset
  digits , # This is the number of decimal places to round off the data
  loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "The important concepts here are from the Knowledge and Data chapter of your coursepack.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Lack of Evidence: Reasoning the contrary is true because of a lack of evidence. Anecdotal Evidence: Generalizing on the grounds of just a few examples. Correlation/Causation: Reasoning that two things happening together means either causes the other." # This is student feedback, visible after the exam
)
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 6 + which.max(points) # This is the row index of the correct answer
  data1 <- sample(c(listen1, listen2, listen3, listen4, listen5,
                    listen6, listen7, listen8, listen9), size = 1) # generating vector for debate topic options 
  corr.ans <- if(data1 == listen1 | data1 == listen2 | data1 == listen3){"... Committed the Lack of Evidence Fallacy"}
  else{if(data1 == listen4 | data1 == listen5 | data1 == listen6){"... Committed the Anecdotal Evidence Fallacy"}
    else{if(data1 == listen7 | data1 == listen8 | data1 == listen9){"... Committed the Correlation Equals Causation Fallacy"}}} # This is the correct answer to the question
  ans.txt <- if(corr.ans == "... Committed the Lack of Evidence Fallacy")
  {sample(c("... Conceptualized a Study's Problem", "... Designed a Study",
            "... Committed the Anecdotal Evidence Fallacy", "Operationalized a Study's Problem",
            "... Committed the Correlation Equals Causation Fallacy", "... Collected Data for a Study",
            "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
          size = answers)}
  else{if(corr.ans == "... Committed the Anecdotal Evidence Fallacy")
  {sample(c("... Conceptualized a Study's Problem", "... Designed a Study",
            "... Committed the Lack of Evidence Fallacy", "... Operationalized a Study's Problem",
            "... Committed the Correlation Equals Causation Fallacy", "... Collected Data for a Study",
            "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
          size = answers)}
    else{if(corr.ans == "... Committed the Correlation Equals Causation Fallacy")
    {sample(c("... Conceptualized a Study's Problem", "... Designed a Study",
              "... Committed the Anecdotal Evidence Fallacy", "... Operationalized a Study's Problem",
              "... Committed the Lack of Evidence Fallacy", "... Collected Data for a Study",
              "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
            size = answers)}}} # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, collapse = "", sep = ""),
               points.per.q, difficulty, points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",6), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options # Indexing and storing all the content
}
questions <- questions[((9+answers)):((8+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

FallaciesMC1() # creating the csv file