##### RegPESEMC1 #####
library(gridExtra) # the pacakge needed for this particular question to create multiple grid-level plots in a page
RegPESEMC1= function(
title = "MeanPESEMC1", # Question-bank title that will be easily viewable in e-learning
  n = 200, # Number of questions to generate
  type = "MC", # The question type, one of many possible types on e-learning
  answers = 5, # Number of answers per MC question
  points.per.q = 4, # Number of points the question is worth (on the test)
  difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A new cardiovascular medication was administered to a group of subjects. The subjects' heartrates (bpm) and weights (lbs) were recorded above after the medication was administered. The standard deviation of the heartrates was ",
quest.txt2 = ", and the standard deviation of the weights was ",
quest.txt3 = ". The correlation coefficient between heartrate and weight was ",
quest.txt4 = ". What is the ",
quest.txt5 = " for the slope of the regression line heartrate (y) vs. weight (x) for all who receive this medicine?", # The above 5 question texts are static text for the full question
digits = 2, # This is the number of decimal places to round off the data
loc.path = "Images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "You'll need a formula from your chapter on regression. Are you looking for center or spread? Pick the closest answer.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Point Estimate = r*sy/sx. SE = sqrt((1-r^2)/(n-2))*sy/sx" # This is student feedback, visible after the exam
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
  decis <- sample(6:10, size = 1)
  BPM <- sample(80:140, size = decis) # randomly generating the heartrates values for the subjects 
  LBS <- sample(110:230, size = decis) # randomly generating the weight values for the subjects
  data <- data.frame(BPM, LBS, stringsAsFactors = FALSE) # generating a data frame combining both the bpm and lbs values
  data1 <- round(sd(BPM), digits) # calculating the standard deviation of heartrates
  data2 <- round(sd(LBS), digits) # calculating the standard deviation of weights
  data3 <- round(cor(BPM, LBS), digits) # calculating the correlation between two variables
  data4 <- sample(c("point estimate", "standard error"), size = 1) # creating the option for question of interest 
  corr.ans <- if(data4 == "point estimate"){round(data3*data1/data2,
                                                  digits = digits)}
  else{round(sqrt((1-data3^2)/(decis-2))*(data1/data2),
             digits = digits)} # This is the correct answer to the question
  up.min <- corr.ans + .12 # This is the minimum value for incorrect answers above the correct answer
  down.max <- corr.ans - .12 # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if((corr.ans <= -.1) & (data4 == "point estimate")){seq(up.min, up.min + .4, 10^-digits)}
                    else{if((corr.ans >= .3) & (data4 == "point estimate")){seq(down.max - .4, down.max, 10^-digits)}
                      else{if((corr.ans <= .1) & (data4 == "standard error")){seq(up.min, up.min + .4, 10^-digits)}
                        else{if((corr.ans >= .3) & (data4 == "standard error")){seq(down.max - .4, down.max, 10^-digits)}
                          else{if((corr.ans > .1) & (corr.ans < .3) & (data4 == "standard error")){
                            c(seq(down.max - .4, down.max, 10^-digits), seq(up.min, up.min + .4, 10^-digits))}
                            else{c(seq(down.max - .4, down.max, 10^-digits),
                                   seq(up.min, up.min + .4, 10^-digits))}}}}},
                    size = answers) # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3, quest.txt4, data4, quest.txt5, sep = ""),
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

RegPESEMC1() # creating the csv file
