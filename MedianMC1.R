##### MedianMC1 #####
MedianMC1= function(
title = "MedianMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt = "What is the median of the following dataset?  Data: ", # This is static text for the question
digits = 1, # This is the number of decimal places to round off the data
dat.size = c(5,6), # This is the vector of number of values to be randomly generated for the dataset
loc.path , # This is the local path used to store any randomly generated image files
  e.path ,  # This is the path on e-learning used to store any above-implemented image files
  hint = "Sort the data first.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Sort and find the middle number, or take the average of the two middle numbers." # This is student feedback, visible after the exam
)
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  dat.size1 = sample(dat.size, size = 1) # randomly generating sample size of median data
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 6 + which.max(points) # This is the row index of the correct answer
  data <- round(rnorm(dat.size1,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size1,df=30)), digits = digits) # This is how we're randomly generating data for this question
  corr.ans <- round(median(data), digits = digits) # This is the correct answer to the question
  up.min <- round(corr.ans + sd(data)/8, digits = digits) # This is the minimum value for incorrect answers above the correct answer
  down.max <- round(corr.ans - sd(data)/8, digits = digits) # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- round(sample(c(if((dat.size1 %% 2) == 0){sort(data)[ceiling((dat.size1+1)/2)]}else{sort(data)[1+(dat.size1+1)/2]},
                            if((dat.size1 %% 2) == 0){sort(data)[floor((dat.size1+1)/2)]}else{sort(data)[(dat.size1+1)/2-1]},
                            seq(corr.ans - 2*sd(data), down.max, 10^-digits),
                            seq(up.min, corr.ans + 2*sd(data), 10^-digits)),
                          size = answers),
                   digits = digits) # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt, paste(as.character(data),
                                                    collapse=",  ",sep=""),
                                   collapse = ""),
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

MedianMC1() # creating the csv file