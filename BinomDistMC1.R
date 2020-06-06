##### BinomDistMC1 #####
BinomDistMC1= function(
title = "BinomDistMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A student must answer ",
quest.txt2 = " multiple choice questions for a test, but the student did not study well. There are ",
quest.txt3 = " answers per question but only one is correct. If the student randomly guesses on each question, what is the probability that the student answers ",
quest.txt4 = " questions correctly?", # The above 4 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 3, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "First, you need to find the number of successes, the number of trials, and infer the probability p from the context of this question. Apply the correct formula and round and select the closest answer.", # This is a student hint, visible to them during the exam on e-learning
feedback = "1: Find successes x. 2: Find trials n. 3: Infer p. 4: Apply exact binomial formula." # This is student feedback, visible after the exam
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
  data1 <- sample(6:10, size = 1) # randomly generating number of trials needed for the question 
  data2 <- sample(3:6, size = 1) # randomly generating the number of answers per question for the given problem
  decis1 <- sample(2:4, size = 1) # randomy generating the number of successes needed for the question
  decis2 <- sample(c("or fewer", "fewer than"), size = 1) # generating decision vector for the question
  data3 <- if(decis2 == "or fewer"){paste(decis1, decis2, sep = " ")}
  else{paste(decis2, decis1, sep = " ")}
  corr.ans <- round(if(decis2 == "or fewer"){pbinom(decis1, data1, 1/data2)}
                    else{pbinom((decis1-1), data1, 1/data2)},
                    digits = digits) # this is the correct answer to the question based on the exact binomial formula
  up.min <- round(corr.ans + .05, digits) # This is the minimum value for incorrect answers above the correct answer
  down.max <- round(corr.ans - .05, digits) # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
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

BinomDistMC1() # creating the csv file
