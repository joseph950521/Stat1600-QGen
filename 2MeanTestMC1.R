##### 2MeanTestMC1 #####
twoMeanTestMC1= function(
title = "2MeanTestMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A researcher collects numeric data across two groups of subjects. The first group is composed of ",
quest.txt2 = " subjects, and the second group of ",
quest.txt3 = " subjects. The sample mean of the first group is ",
quest.txt4 = " and its standard deviation is ",
quest.txt5 = ". The sample mean of the second group is ",
quest.txt6 = " and its standard deviation is ",
quest.txt7 = ". What is the Z test statistic of a hypothesis test on the difference between the two means (first group - second group)?", # The above 7 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 3, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "You need to calculate the Z test statistic. Don't take the absolute value. Pick the closest answer.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Did you use (xbar1 - xbar2)/SE?" # This is student feedback, visible after the exam
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
  data1 <- sample(15:30, size = 1) # randomly generating sample size for sample 1
  data2 <- sample(15:30, size = 1) # randomly generating sample size for sample 2
  data3 <- sample(80:100, size = 1) # randomly generating data of size 1 for sample mean of sample 1
  data4 <- sample(20:30, size = 1) # randomly generating data of size 1 for sample standard deviation of sample 1
  data5 <- sample(80:100, size = 1) # randomly generating data of size 1 for sample mean of sample 2
  data6 <- sample(20:30, size = 1) # randomly generating data of size 1 for sample standard deviation of sample 2
  corr.ans <- round((data3 - data5)/sqrt(data4^2/data1 + data6^2/data2), digits) # this is the correct answer to the question
  up.min <- round(corr.ans + .25, digits) # This is the minimum value for incorrect answers above the correct answer
  down.max <- round(corr.ans - .25, digits) # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans < -1.9){seq(up.min, 2.5, 10^-digits)}
                    else{if(corr.ans > 1.9){seq(-2.5, down.max, 10^-digits)}
                      else{c(seq(-2.5, down.max, 10^-digits),
                             seq(up.min, 2.5, 10^-digits))}},
                    size = answers) # These are randomly generated incorrect answers.
  ans.txt <- round(ans.txt, digits = digits) #rounding up the incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5, data5, quest.txt6,
                                   data6, quest.txt7,
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

twoMeanTestMC1() # creating the csv file
