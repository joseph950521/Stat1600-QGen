##### 2MeanPESEMC1 #####
library(gridExtra) # the pacakge needed for this particular question to create multiple grid-level plots in a page
twoMeanPESEMC1= function(
title = "2MeanPESEMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "Both a new cardiovascular medication and placebo were administered to two separate groups of subjects. The subjects' heartrates (bpm) were monitored and recorded in the above table. The standard deviation of the medicated group is ",
quest.txt2 = ", and the standard deviation of the placebo group is ",
quest.txt3 = ". What is the ",
quest.txt4 = " of the difference between the mean heartrate of medication vs.the mean heartrate of placebo groups?", # The above 4 question texts are static text for the full question
digits = 2, # This is the number of decimal places to round off the data
loc.path = "Images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "Are the groups independent or dependent? Are you looking for center or spread? Pick the closest answer.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Point Estimate = mean(medication) - mean(placebo). SE = sqrt(smed^2/nmean + splac^2/nplac."
) # This is student feedback, visible after the exam
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 7 + which.max(points) # This is the row index of the correct answer
  decis <- sample(6:8, size = 1) # randomly assigning sample size number
  Medication <- sample(80:140, size = decis)
  Placebo <- sample(90:160, size = decis)
  data <- data.frame(Medication, Placebo, stringsAsFactors = FALSE) # This is how we're randomly generating data table for this question
  data1 <- round(sd(Medication), digits) # generating sample standard deviation of medication
  data2 <- round(sd(Placebo), digits) #generating sample standard deviation of placebo
  data3 <- sample(c("point estimate", "standard error"), size = 1)
  corr.ans <- if(data3 == "point estimate"){round(mean(Medication) - mean(Placebo),
                                                  digits = digits)}
  else{round(sqrt(data1^2/decis + data2^2/decis),
             digits = digits)} # This is the correct answer to the question
  up.min <- corr.ans + .2 # This is the minimum value for incorrect answers above the correct answer
  down.max <- corr.ans - .2 # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans <= -15){seq(up.min, up.min + 5, 10^-digits)}
                    else{if(corr.ans >= 15){seq(down.max - 5, down.max, 10^-digits)}
                      else{c(seq(down.max - 5, down.max, 10^-digits),
                             seq(up.min, up.min + 5, 10^-digits))}},
                    size = answers) # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3, quest.txt4, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 80*ncol(data)) # creating the image files in the designated place
  p <- tableGrob(data) #Creating the summary statistics for the graphical item
  grid.arrange(p) #arranging all the summary statistics for the graphical items in one location
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

twoMeanPESEMC1() # creating the csv file