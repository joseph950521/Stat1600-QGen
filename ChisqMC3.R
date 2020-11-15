##### ChisqMC3 #####
library(gridExtra) # the pacakge needed for this particular question to create multiple grid-level plots in a page
ChisqMC3= function(
title = "ChisqMC3", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "Researchers collect data on two variables, the country of origin and religion of their subjects. There are ",
quest.txt2 = " countries and ",
quest.txt3 = " religions in the study. The researchers run a chi square test of association between country and religion. Using the table above, determine the critical value for this test.", # The above 3 question texts are the static text for the full question
digits = 2, # This is the number of decimal places to round off the data
loc.path = "/Users/josephyang/Desktop/School Stuff/STAT 1600/Course Development/Question Generators/Stat1600-QGen/ChisqMC3 images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "You need the formula for degrees of freedom from your chapter on chi squares.", # This is a student hint, visible to them during the exam on e-learning
feedback = "df = (#countries - 1)*(#religions - 1)" # This is student feedback, visible after the exam
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
  data1 <- sample(2:4, size = 1) # randomly generating the number of countries
  data2 <- sample(2:4, size = 1) # randomly generating the number of religions
  DF <- 1:10
  CV <- round(qchisq(.95, 1:10), digits = digits) # creating the critical values for different degrees of freedom
  data <- rbind(DF, CV) # joining each critical value with respective degrees of freedom
  corr.ans <- CV[(data1-1)*(data2-1)] # This is the correct answer to the question
  ans.txt <- sample(CV[CV != corr.ans], size = answers) # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, sep = ""),
               points.per.q, difficulty, paste(e.path, title, ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, title, ".jpeg", sep = ""),
       height = 22*nrow(data), width = 43*ncol(data)) # creating the image files in the designated place
  p <- tableGrob(data) # Creating the summary statistics for the graphical item
  grid.arrange(p)  # arranging all the summary statistics for the graphical items in one location
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

ChisqMC3()# creating the csv file
