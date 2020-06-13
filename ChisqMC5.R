##### ChisqMC5 #####
library(gridExtra) # the pacakge needed for this particular question to create multiple grid-level plots in a page
ChisqMC5= function(
title = "ChisqMC5", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "The above table contains frequency data across two categorical variables. Given that the ",
quest.txt2 = " cell's expected count is ",
quest.txt3 = ", calculate that cell's contribution to the summation in a chi square test statistic.", # The above 3 question texts are static text for the full question
digits = 2, # This is the number of decimal places to round off the data
loc.path = "Images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "You need the formula for the chi square test statistic from your chapter on chi squares. You don't need to calculate the entire chi square test statistic. Pick the closest answer.", # This is a student hint, visible to them during the exam on e-learning
feedback = "(E - O)^2/E" # This is student feedback, visible after the exam
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
  decis1 <- sample(3:7, size = 1) # creating the highest limit of number of groups
  decis2 <- sample(3:7, size = 1) # creating the highest limit of number of levels
  rn <- c()
  cn <- c()
  for(d in 1:decis1){rn[d] <- paste("Group", d)} # reporting the group number in the table in each row
  for(e in 1:decis2){cn[e] <- paste("Level", e)} # reporting the level number in the table in each column
  data <- matrix(sample(20:200, size = decis1*decis2), nrow = decis1, ncol = decis2,
                 dimnames = list(rn, cn)) # randomly creating the cell values in a matrix
  data <- addmargins(data, FUN = list(Total = sum), quiet = TRUE) # creating boundaries around and within the matrix to represent it as a table
  decis3 <- c(sample(1:decis1, size = 1), sample(1:decis2, size = 1)) # collecting all the group and level numbers in a vector
  data1 <- paste(rn[decis3[1]], cn[decis3[2]], sep = ", ") # randomly choosing a cell number
  data2 <- round((data[decis3[1], decis2 + 1])*(data[decis1 + 1, decis3[2]])/sum(data[-(decis1 + 1), decis2 + 1]), digits = digits) # creating the expected count of the cell
  o <- data[decis3[1], decis3[2]] # creating the observed count
  corr.ans <- round((data2 - o)^2/data2, digits = digits) # This is the correct answer to the question 
  up.min <- corr.ans + .3 # This is the minimum value for incorrect answers above the correct answer
  down.max <- corr.ans - .3 # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans <= 10){seq(up.min, up.min + 10, 10^-digits)}
                    else{if(corr.ans >= 40){seq(down.max - 20, down.max, 10^-digits)}
                      else{c(seq(down.max - 10, down.max, 10^-digits),
                             seq(up.min, up.min + 20, 10^-digits))}},
                    size = answers) # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 65*ncol(data)) # creating the image files in the designated place
  p <- tableGrob(data) # Creating the summary statistics for the graphical item
  grid.arrange(p) # arranging all the summary statistics for the graphical items in one location
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

ChisqMC5() # creating the csv file