##### PropCIMC2 #####
PropCIMC2= function(
title = "PropCIMC2", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 2, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A student conducts a survey for her class project. She asks ",
quest.txt2 = " other students a series of questions about their study habits. She knows from previous research that ",
quest.txt3 = " of all students study for 4 or more hours per day. Based on her sample data, she calculates a 95% confidence interval of ",
quest.txt4 = ". Was her confidence interval successful?", # The above 4 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "This is an interpretive question. No calculation is necessary. Consider each bound inclusive.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Does the CI contain the true population proportion?" # This is student feedback, visible after the exam
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
  data1 <- sample(20:40, size = 1) # randomly generating data of size 1 for sample size for the calculation
  data2 <- sample(seq(.4,.6,.01), size = 1)
  sampprop <- sample(seq(.1,.9,.01), size = 1) # randomly generating data of size 1 for sample proportion for the calculation
  lb <- sampprop - 1.96*sqrt(sampprop*(1-sampprop)/data1) # creating lower bound of the 95% confidence interval
  ub <- sampprop + 1.96*sqrt(sampprop*(1-sampprop)/data1) # creating upper bound of the 95% confidence interval
  data3 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "") # rounding up the lower and upper bound of 95% confidence interval
  corr.ans <- if((data2 <= ub) & (data2 >= lb)){"Yes"}else{"No"} # this is the correct answer to the question
  ans.txt <- c(if(corr.ans == "Yes"){rep("No", 2)}else{rep("Yes", 2)}) # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
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
PropCIMC2() # creating the csv file