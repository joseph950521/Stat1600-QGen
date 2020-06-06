##### BinomApproxMC3 #####
BinomApproxMC3= function(
title = "BinomApproxMC3", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 2, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A brewery pulls ",
quest.txt2 = " bottles of beer to sample them for quality control. In the past, there has been a ",
quest.txt3 = " probability that a randomly selected bottle of beer is non-defective. Managment would like to know what the probability that ",
quest.txt4 = " bottles of beer in this particular sample are non-defective. Is it appropriate to apply normal approximation in order to solve this problem?", # The above 4 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files 
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "This is mainly an interpretive question. You do not need to calculate the probability, although some other small calculations may be necessary.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Did you check the size of np and nq?" # This is student feedback, visible after the exam
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
  data1 <- sample(50:160, size = 1) # randomly generating sample size for the question
  data2 <- sample(seq(.942, .962, .001), size = 1) # randomly generating population probability for the question
  decis1 <- sample((data1-40):(data1-10), size = 1)  # randomly generating the number of successes for the question
  decis2 <- sample(c("or more", "more than"), size = 1)   # generating decision vector for the question
  data3 <- if(decis2 == "or more"){paste(decis1, decis2, sep = " ")}
  else{paste(decis2, decis1, sep = " ")}
  corr.ans <- if((data1*data2 > 5) & (data1*(1-data2) > 5)){"Yes"}else{"No"} # this is the correct answer based on the condition of 'np' and 'nq'
  ans.txt <- if(corr.ans == "Yes"){rep("No", 2)}else{rep("Yes", 2)} # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",6), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans  # This is imputing the correct answer at the appropriate row index
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
}
questions <- questions[(9+answers):((8+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

BinomApproxMC3() # creating the csv file