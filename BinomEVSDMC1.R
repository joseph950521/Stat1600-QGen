##### BinomEVSDMC1 #####
BinomEVSDMC1= function(
title = "BinomEVSDMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A binomial process has ",
quest.txt2 = " trials and a probability of success of ",
quest.txt3 = ". What is the ",
quest.txt4 = " of this process' distribution?", # The above 4 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files 
  hint = "Find the appropriate formula from the coursepack's chapter on the binomial distribution.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Expected Value = np. Standard Deviation = sqrt(npq)." # This is student feedback, visible after the exam
)
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 6 + which.max(points) # This is the row index of the correct answer
  data1 <- sample(500:1500, size = 1) # randomly generating number of trials needed for the question 
  data2 <- sample(seq(.01, .99, 10^-digits), size = 1) # randomly generating population probability for the question
  data3 <- sample(c("expected value", "standard deviation"), size = 1) # generating specific statistic calculation option for the question
  corr.ans <- if(data3 == "expected value"){data1*data2}else{sqrt(data1*data2*(1-data2))} # this is the correct answer to the question based on the formula of expected value or standard deviation
  up.min <- round(corr.ans + 5, digits) # This is the minimum value for incorrect answers above the correct answer
  down.max <- round(corr.ans - 5, digits) # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans <= 15){seq(up.min, 100, 10^-digits)}
                    else{if(corr.ans >= 1000){seq(800, down.max, 10^-digits)}
                      else{c(seq(10^-digits, down.max, 10^-digits),
                             seq(up.min, up.min + 100, 10^-digits))}},
                    size = answers) # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",6), round(ans.txt, digits = digits), rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- round(corr.ans,digits = digits) # This is imputing the correct answer at the appropriate row index
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
}
questions <- questions[(9+answers):((8+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

BinomEVSDMC1() # creating the csv file
