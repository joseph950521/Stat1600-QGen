##### QuartMC1 #####
QuartMC1= function(
title = "QuartMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "What is the ",
quest.txt2 = " of the following dataset?    ", # these above 2 question texts are static texts for the full question
digits = 1, # This is the number of decimal places to round off the data
dat.size = 11:15, # This is the number of values to be randomly generated for the dataset
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "Sort the data first.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Sort the data, then find either the .25(n + 1)st number (Q1) or the .75(n + 1)st number (Q3)." # This is student feedback, visible after the exam
)
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  dat.size1 = sample(dat.size, size = 1) # randomly generating sample size for the question
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 6 + which.max(points) # This is the row index of the correct answer
  data1 <- sample(c("first quartile", "third quartile"), size = 1) # generating the vector of quartile option for the question 
  data <- round(rnorm(dat.size1,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size1,df=30)), digits = digits) # randomly generating the values of dataset
  corr.ans <- if(data1 == "first quartile"){round(mean(c(sort(data)[ceiling((dat.size1+1)/4)],
                                                         sort(data)[floor((dat.size1+1)/4)])),
                                                  digits = digits)}
  else{round(mean(c(sort(data)[ceiling(3*(dat.size1+1)/4)],
                    sort(data)[floor(3*(dat.size1+1)/4)])),
             digits = digits)} # This is the correct answer to the question according to the equation for calculation of first and third quartile
  up.min <- round(corr.ans + sd(data)/8, digits = digits) # This is the minimum value for incorrect answers above the correct answer
  down.max <- round(corr.ans - sd(data)/8, digits = digits) # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- round(sample(c(median(data), mean(data),
                            if(data1 == "first quartile")
                            {if((dat.size1 %% 2) == 0){c(sort(data)[ceiling((dat.size1+1)/4)],
                                                         sort(data)[floor((dat.size1+1)/4)])}
                              else{c(sort(data)[1+(dat.size1+1)/4], sort(data)[(dat.size1+1)/4-1])}},
                            if(data1 == "third quartile")
                            {if((dat.size1 %% 2) == 0){c(sort(data)[ceiling(3*(dat.size1+1)/4)],
                                                         sort(data)[floor(3*(dat.size1+1)/4)])}
                              else{c(sort(data)[1+3*(dat.size1+1)/4], sort(data)[3*(dat.size1+1)/4-1])}},
                            seq(corr.ans - 3*sd(data), down.max, 10^-digits),
                            seq(up.min, corr.ans + 3*sd(data), 10^-digits)),
                          size = answers),
                   digits = digits) # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   paste(as.character(data), collapse=",  ",sep=""),
                                   collapse = "", sep = ""),
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

QuartMC1() # creating the csv file