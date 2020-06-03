##### 2PropTestMC5 #####
twoPropTestMC5= function(
title = "2PropTestMC5", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "You want to see whether you or your friend is the better cook, so you ask your mutual friends to repeatedly sample both of your meals and decide whether they are 'great' or 'not great'. You cook ",
quest.txt2 = " great meals. Your friend cooks ",
quest.txt3 = " great meals. You cooked a total of ",
quest.txt4 = " meals, and your friend cooked a total of ",
quest.txt5 = " meals. What is the Z test statistic for a hypothesis test on the difference between proportions of great meals (yours - your friend's)?", # The above 5 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "You need to calculate the Z test statistic. Don't take the absolute value. Pick the closest answer.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Did you use (phat1 - phat2)/SE?" # This is student feedback, visible after the exam
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
  data1 <- sample(60:75, size = 1) # randomly generating the number of succesess of sample 1
  data2 <- sample(50:65, size = 1) # randomly generating the number of succesess of sample 2
  data3 <- sample(80:150, size = 1) # randomly generating sample size for sample 1
  data4 <- sample(80:150, size = 1) # randomly generating sample size for sample 2
  corr.ans <- round((data1/data3-data2/data4)/sqrt(data1/data3*(1-data1/data3)/data3+data2/data4*(1-data2/data4)/data4), digits) # this is the correct answer to the question
  up.min <- round(corr.ans + .05, digits) # This is the minimum value for incorrect answers above the correct answer
  down.max <- round(corr.ans - .05, digits) # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans < -6){seq(up.min, 12, 10^-digits)}
                    else{if(corr.ans > 12){seq(-6, down.max, 10^-digits)}
                      else{c(seq(-6.9, down.max, 10^-digits),
                             seq(up.min, 12.8, 10^-digits))}},
                    size = answers) # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
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

twoPropTestMC5() # creating the csv file