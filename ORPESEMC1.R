##### ORPESEMC1 #####
library(gridExtra) # the pacakge needed for this particular question to create multiple grid-level plots in a page
ORPESEMC1= function(
title = "ORPESEMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "Both a new medication and placebo were administered to two separate groups of subjects. The subjects were monitored for recovery from an illness and the results were recorded in the 2x2 contingency table above. What is the ",
quest.txt2 = " for recovery in subjects who received the medication vs. those who received the placebo?", # The above 2 question texts are static text for the full question
Treatments = c("Medication", "Placebo"), #creating a vector of the treatment characters in the question
digits = 2, # This is the number of decimal places to round off the data
loc.path = "/Users/josephyang/Desktop/School Stuff/STAT 1600/Course Development/Question Generators/Stat1600-QGen/ORPESEMC1 images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "You can follow your a, b, c, d formula given in lectures and the coursepack.", # This is a student hint, visible to them during the exam on e-learning
feedback = "OR = ad/(bc). SE = sqrt(1/a + 1/b + 1/c + 1/d)" # This is student feedback, visible after the exam
)
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 7 + which.max(points) # This is the row index of the correct answer
  data1 <- sample(c("odds ratio", "standard error of the natural log of the odds ratio"), size = 1)
  Recovered <- c(sample(50:100, size = 1), sample(25:75, size = 1))
  Unrecovered <- sample(100:200, size = 2)
  data <- data.frame(Treatments, Recovered, Unrecovered, stringsAsFactors = FALSE) # This is how we're randomly generating data frame for this question
  corr.ans <- if(data1 == "odds ratio"){round(data[1,2]*data[2,3]/(data[1,3]*data[2,2]),
                                              digits = digits)}
  else{round(sqrt(1/data[1,2] + 1/data[2,2] + 1/data[1,3] + 1/data[2,3]),
             digits = digits)} # this is the correct answer to the question
  up.min <- corr.ans + .08 # This is the minimum value for incorrect answers above the correct answer
  down.max <- corr.ans - .08 # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans <= .1){seq(up.min, 1, 10^-digits)}
                    else{if(corr.ans >= 1.3){seq(.01, down.max, 10^-digits)}
                      else{c(seq(10^-digits, down.max, 10^-digits),
                             seq(up.min, up.min + 2.2, 10^-digits))}},
                    size = answers) # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 30*nrow(data), width = 81*ncol(data)) # creating the image files in the designated place
  p <- tableGrob(data) #Creating the summary statistics for the graphical item
  grid.arrange(p) #arranging all the summary statistics for the graphical items in one location
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

ORPESEMC1() # creating the csv file