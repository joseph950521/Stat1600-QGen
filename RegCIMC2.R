##### RegCIMC2 #####
RegCIMC2= function(
title = "RegCIMC2", # Question-bank title that will be easily viewable in e-learning
n = 200, # Question-bank title that will be easily viewable in e-learning
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A researcher compares variables x and y using linear regression. She calculates a 95% confidence interval of ",
quest.txt2 = " for the slope of the regression line. Which of the following statistical inferences should she make?", # The above 2 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files 
  hint = "This is an interpretive question. No calculation is necessary. Consider each bound inclusive.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Does the CI contain 0?" # This is student feedback, visible after the exam
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
  lb <- sample(seq(-.2, .11, 10^-digits), size = 1) # creating lower bound of the 95% confidence interval
  ub <- sample(seq(lb + .03, .2), size = 1) # creating upper bound of the 95% confidence interval
  data1 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "") # showing the 95% confidence interval by rounding off lower and upper bound
  corr.ans <- if((lb <= 0) & (ub >= 0)){"The slope is insignificant at the 95% confidence level."}
  else{"The slope is significant at the 95% confidence level."} # this is the correct answer to the question
  ans.txt <- if(corr.ans == "The slope is insignificant at the 95% confidence level.")
  {c("The slope is significant at the 95% confidence level.",
     "The sample slope is not contained within the confidence interval.",
     "Zero is not contained within the confidence interval.",
     "The result is significant, thus the p-value is less than 0.05.")}
  else{c("The slope is insignificant at the 95% confidence level.",
         "The sample slope is contained within the confidence interval.",
         "Zero is contained within the confidence interval.",
         "The result is insignificant, thus the p-value is greater than 0.05.")} # These are randomly generated incorrect answers.
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

RegCIMC2() # creating the csv file