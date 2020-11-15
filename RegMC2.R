##### RegMC2 #####
RegMC2= function(
title = "RegMC2", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 3, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "The regression of two variables Y vs. X is depicted above. Determine whether the slope is positive, negative, or zero.", # The above question text is the static text for the full question
digits = 2, # This is the number of decimal places to round off the data
loc.path = "/Users/josephyang/Desktop/School Stuff/STAT 1600/Course Development/Question Generators/Stat1600-QGen/RegMC2 images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "No calculation is necessary. Just interpret the above graph.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Lines tilted up from left to right are positive, tilted down are negative, and flat liens are zero." # This is student feedback, visible after the exam
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
  decis <- sample(1:3, size = 1)
  X <- sample(1:40, size = 10) # # randomly generating the X values
  Y <- if(decis == 1){sample(10:20, size = 1) + sample(seq(0.25, 5, by = 0.25), size = 1)*X + rnorm(10,0,3)}
  else{if(decis == 2){sample(10:20, size = 1) - sample(seq(0.25, 5, by = 0.25), size = 1)*X + rnorm(10,0,3)}
    else{rep(sample(10:20, size = 1), length(X))}} # randomly generating the Y values and changing the values depending upon condition
  corr.ans <- if(decis == 1){"Positive"}else{if(decis == 2){"Negative"}else{"Zero"}} # This is the correct answer to the question
  ans.txt <- if(decis == 1){c("Negative", "Zero", "Neither of These")}else{if(decis == 2){c("Positive", "Zero", "Neither of These")}else{c("Positive", "Negative", "Neither of These")}} # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, quest.txt1,
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = "")) # creating the image files in the designated place
  plot(X, Y, xlim = c(-7,7), ylim = c(3,28), type = "n"); abline(lm(Y~X)); title("Y vs. X"); # creating a scatter plot of X and Y values with fixed limit points and then drawing a linear regression line within the plot
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

RegMC2() # creating the csv file