##### PropEVSDMC1 #####
PropEVSDMC1= function(
title = "PropEVSDMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "In general, ",
quest.txt2 = " of all students default on their student loan debt. A sample of ",
quest.txt3 = " students with student loan debt is randomly selected and it's found that ",
quest.txt4 = " of them default. What is the ",
quest.txt5 = "?", # The above 5 question texts are static text for the full question
dat.size = 1, # This is the number of values to be randomly generated for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "Find the appropriate formula from the coursepack's chapter on the distribution of the sample proportion.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Sample proportion = x/n. Standard Error = sqrt(pq/n). Population proportion = p." # This is student feedback, visible after the exam
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
  data1 <- sample(seq(.15, .30, 10^-digits), size = 1) # randomly generating population proportion
  data2 <- sample(500:600, size = 1) # randomly generating sample size number
  data3 <- sample((data2 - 450):(data2 - 470), size = 1) # randomly generating the number of sample success
  data4 <- sample(c("sample proportion", "population proportion", "standard error of the sample proportion",
                    "standard deviation of the sample proportion's sampling distribution"), size = 1) # randomly generating any one of three statistics needed to be calculated in this question
  corr.ans <- if(data4 == "sample proportion"){data3/data2}
  else{if(data4 == "population proportion"){data1}
    else{if(data4 == "standard error of the sample proportion"){sqrt((data3/data2*(1-data3/data2))/data2)}
      else{sqrt((data1*(1-data1))/data2)}}} # This is the correct answer to the question
  up.min <- round(corr.ans + .02, digits) # This is the minimum value for incorrect answers above the correct answer
  down.max <- round(corr.ans - .02, digits) # This is the maximum value for incorrect answers below the correct answer
  ans.txt <- sample(if(corr.ans <= .03){seq(up.min, .2, 10^-digits)}
                    else{if(corr.ans >= .2){seq(.1, down.max, 10^-digits)}
                      else{c(seq(10^-digits, down.max, 10^-digits),
                             seq(up.min, .3, 10^-digits))}},
                    size = answers) # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
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

PropEVSDMC1() # creating the csv file
