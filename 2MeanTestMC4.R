##### 2MeanTestMC4 #####
twoMeanTestMC4= function(
title = "2MeanTestMC4", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 2, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A researcher runs a hypothesis test on the difference between two means. She finds the Z test statistic is ",
quest.txt2 = " What is the appropriate decision at the 5% significance level?", # The above 2 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "Is the test statistic bigger or smaller than 1.959964?", # This is a student hint, visible to them during the exam on e-learning
feedback = "Reject when the test statistic is >= 1.959964." # This is student feedback, visible after the exam
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
  dec <- sample(1:2, 1)
  if(dec == 1){data1 <- round(sample(seq(1.96, 10, 10^-digits), size = 1), digits)}
  else{data1 <- round(sample(seq(-1.9, 1.9, 10^-digits), size = 1), digits)} #randomly generating test statistic value
  corr.ans <- if(data1 >= 1.96){"Reject the null hypothesis."} 
  else{"Fail to reject the null hypothesis."} # this is the correct answer to the question based on the condition
  ans.txt <- rep(if(corr.ans == "Fail to reject the null hypothesis."){"Reject the null hypothesis."}else{"Fail to reject the null hypothesis."}, answers) # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",6), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
}
questions <- questions[(9+answers):((8+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

twoMeanTestMC4() # creating the csv file