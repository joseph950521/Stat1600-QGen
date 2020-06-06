##### BinomApporxMC2 #####
BinomApproxMC2= function(
title = "BinomApproxMC2", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A brewery pulls ",
quest.txt2 = " bottles of beer to sample them for quality control. In the past, there has been a ",
quest.txt3 = " probability that a randomly selected bottle of beer is non-defective. Assuming this trend continues (independence), what is the probability that ",
quest.txt4 = " bottles of beer in this particular sample are non-defective?", # The above 4 question texts are static texts for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "You'll need to apply the normal approximation here.", # This is a student hint, visible to them during the exam on e-learning
feedback = "1: Find successes x. 2: Find trials n. 3. Find probability p. 4. Use the normal approximation formula with appropriate continuity correction. 5. Use the z table to find the probability (make sure you use 1 - probability to get upper tail)." # This is student feedback, visible after the exam
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
  data1 <- sample(905:915, size = 1) # randomly generating sample size for the question
  data2 <- sample(seq(.942, .962, .001), size = 1) # randomly generating population probability for the question
  decis1 <- sample(858:871, size = 1) # randomly generating the number of successes for the question
  decis2 <- sample(c("or more", "more than"), size = 1) # generating decision vector for the question
  data3 <- if(decis2 == "or more"){paste(decis1, decis2, sep = " ")}
  else{paste(decis2, decis1, sep = " ")}
  corr.ans <- round(if(decis2 == "or more"){pnorm(round((decis1-.5 - data1*data2)/sqrt(data1*data2*(1-data2)),
                                                        digits = 2),
                                                  lower.tail = F)}
                    else{pnorm(round((decis1+.5 - data1*data2)/sqrt(data1*data2*(1-data2)),
                                     digits = 2),
                               lower.tail = F)},
                    digits = digits)  # this is the correct answer to the question using normal approximation formula with continuity correction
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


BinomApproxMC2() # creating the csv file