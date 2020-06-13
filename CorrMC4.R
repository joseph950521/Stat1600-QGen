##### CorrMC4 #####
library(gridExtra) # the pacakge needed for this particular question to create multiple grid-level plots in a page
CorrMC4= function(
title = "CorrMC4", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "The Z-scores = (X- mean)/SD for two variables X and Y are given above. Calculate the correlation coefficient r between the two variables.", # The above text is static text for the question
digits = 2, # This is the number of decimal places to round off the data
loc.path = "Images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "You'll need to follow the last couple steps of calculating correlation coefficients. Pick the closest answer.", # This is a student hint, visible to them during the exam on e-learning
feedback = "r = sum(Zx*Zy)/(n-1)" # This is student feedback, visible after the exam
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
  decis <- sample(6:8, size = 1) # randomly generating the sample size for the question
  X <- sample(80:140, size = decis) # randomly generating values for X variable
  Y <- sample(90:160, size = decis) # randomly generating values for Y variable
  Zx <- round((X-mean(X))/sd(X), digits) # creating the z-scores of X variable
  Zy <- round((Y-mean(Y))/sd(Y), digits) # creating the z-scores of Y variable
  data <- data.frame(Zx, Zy, stringsAsFactors = FALSE) # creating a data frame of z-scores of both X and Y variable in the question
  corr.ans <- round(sum(Zx*Zy)/(decis-1), digits) # This is the correct answer to the question
  up.min <- corr.ans + .1 # This is the minimum value for incorrect answers above the correct answer
  down.max <- corr.ans - .1 # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans <= -.8){seq(up.min, up.min + 1, 10^-digits)}
                    else{if(corr.ans >= .8){seq(down.max - 1, down.max, 10^-digits)}
                      else{c(seq(down.max - .25, down.max, 10^-digits),
                             seq(up.min, up.min + .25, 10^-digits))}},
                    size = answers) # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, quest.txt1,
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 60*ncol(data)) # creating the image files in the designated place
  p <- tableGrob(data) # Creating the summary statistics for the graphical item
  grid.arrange(p) # arranging all the summary statistics for the graphical items in one location
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

CorrMC4() # creating the csv file