##### CorrMC1 #####
CorrMC1= function(
title = "CorrMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "Four scatterplots of two variables Y vs. X are depicted above. Determine which image depicts the strongest correlation between Y and X.", # This is static text for the question
digits = 2, # This is the number of decimal places to round off the data
loc.path = "/Users/josephyang/Desktop/School Stuff/STAT 1600/Course Development/Question Generators/Stat1600-QGen/CorrMC1 images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "No calculation is necessary. Just interpret the above graph.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Strongest correlations have points closest to a line of best fit." # This is student feedback, visible after the exam
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
  X <- sample(1:30, size = 20); X1 <- X; X2 <- X; X3 <- X; X4 <- X # randomly generating the data points of x-axis for all four scatterplots
  decis <- sample(1:4, size = 1) # Randomly generating indication number for each of four scatterplots
  if(decis == 1)
  {
    Y1 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,30)
    Y2 <- sample(10:20, size = 1) + sample(seq(-6, -3, 1), size = 1)*X + rnorm(length(X),0,2)
    Y3 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(-4, -3, 1), size = 1)*X + rnorm(length(X),0,30)
    Y4 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(2, 3, 1), size = 1)*X + rnorm(length(X), 0, 30)
  }
  else{if(decis == 2)
  {
    Y2 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,30)
    Y3 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,2)
    Y4 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(-4, -3, 1), size = 1)*X + rnorm(length(X),0,30)
    Y1 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(2, 3, 1), size = 1)*X + rnorm(length(X), 0, 30)
  }
    else{if(decis == 3)
    {
      Y3 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,30)
      Y4 <- sample(10:20, size = 1) + sample(seq(-6, -3, 1), size = 1)*X + rnorm(length(X),0,2)
      Y1 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(-4, -3, 1), size = 1)*X + rnorm(length(X),0,30)
      Y2 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(2, 3, 1), size = 1)*X + rnorm(length(X), 0, 30)
    }
      else{if(decis == 4)
      {
        Y4 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,30)
        Y1 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,2)
        Y2 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(-4, -3, 1), size = 1)*X + rnorm(length(X),0,30)
        Y3 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(2, 3, 1), size = 1)*X + rnorm(length(X), 0, 30)
      }}}} # randomly generating the y-axis value depending on the indicator value of the scatterplots
  r1 <- cor(X1, Y1); r2 <- cor(X2, Y2); r3 <- cor(X3, Y3); r4 <- cor(X4, Y4) # generating correlation value for each of the scatterplots
  r <- abs(c(r1,r2,r3,r4)) # vector of absolute values of all the four scatterplots
  corr.ans <- which.max(r) # This is the correct answer to the question
  ans.txt <- 1:4 # Finding the indicator number of the scatterplot having maximum correlation value among all the four 
  ans.txt <- c(ans.txt[ans.txt != corr.ans], "None of the Above") # These are randomly generated incorrect answers
  content <- c(type, ID, ID, quest.txt1,
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = "")) # creating the image files in the designated place
  par(mfrow=c(2,2)) # creating the graphical parameter that two scatterplots are drawn in two rows
  plot(X1, Y1, xlim=c(0,30), ylim = c(-200, 200)); title(1) 
  plot(X2, Y2, xlim=c(0,30), ylim = c(-200, 200)); title(2)
  plot(X3, Y3, xlim=c(0,30), ylim = c(-200, 200)); title(3)
  plot(X4, Y4, xlim=c(0,30), ylim = c(-200, 200)); title(4); # setting up the boundary limits of x-axis and y-axis for each of the scatterplots
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

CorrMC1() # creating the csv file

