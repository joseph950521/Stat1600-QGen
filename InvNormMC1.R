##### InvNormMC1 #####
InvNormMC1= function(
title = "InvNormMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "Given that some normally distributed data has a mean of ",
quest.txt2 = " and a standard deviation of ",
quest.txt3 = ". What is the value x of this dataset where ",
quest.txt4 = "% of all other data values are ", # The above 4 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 1, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "This is an inverted or 'backward' Z-table question. Pick the closest answer.", # This is a student hint, visible to them during the exam on e-learning
feedback = "1: Find the closest probability on the Z-table. 2: Find the Z value. 3: Calculate the value x." # This is student feedback, visible after the exam
)
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0,answers-1),100), replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 6 + which.max(points) # This is the row index of the correct answer
  data1 <- round((rnorm(dat.size,mean=rnorm(1,mean=900,sd=300),sd=100) + (0.25*rt(dat.size,df=30))), digits = digits) # generating the mean for the question
  data2 <- round(runif(dat.size, 65, 150), digits = digits) # randomly generating the standard deviation for the question
  data3 <- round(runif(dat.size, 3, 97), digits = 0) # randomly generating the percentage value for the quesiton
  data4 <- sample(c("less?", "greater?"), size = 1) # generating a vector of option for the left or right side of the normal distribution
  corr.ans <- ifelse(data4 == "less?", (data2*round(qnorm(data3/100), digits = 2) + data1),
                     (data2*round(qnorm(data3/100, lower.tail = F), digits = 2) + data1)) # This is the correct answer to the question
  up.min <- round(corr.ans + data2/3, digits = digits) # This is the minimum value for incorrect answers above the correct answer
  down.max <- round(corr.ans - data2/3, digits = digits) # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans < (data1 - 2*data2)){seq(up.min, (data1 + 4*data2), 10^-digits)}
                    else{if(corr.ans > (data1 + 2*data2)){seq((data1 - 4*data2), down.max, 10^-digits)}
                      else{c(seq((data1 - 4*data2), down.max, 10^-digits),
                             seq(up.min, (data1 + 4*data2), 10^-digits))}},
                    size = answers) # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",6), round(ans.txt, digits = digits), rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- round(corr.ans, digits = digits) # This is imputing the correct answer at the appropriate row index
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
}
questions <- questions[(9+answers):((8+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

InvNormMC1() # creating the csv file