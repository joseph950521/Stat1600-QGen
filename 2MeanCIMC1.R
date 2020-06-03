##### 2MeanCIMC1 #####
twomeanCIMC1= function(
  title = "2MeanCIMC1", # Question-bank title that will be easily viewable in e-learning
  n = 200, # Number of questions to generate
  type = "MC", # The question type, one of many possible types on e-learning
  answers = 5, # Number of answers per MC question
  points.per.q = 4, # Number of points the question is worth (on the test)
  difficulty = 1, # An easily viewable difficulty level on e-learning
  quest.txt1 = "NASA compares the ages of stars in two distinct clusters. A sample from the first cluster has an average age of ",
  quest.txt2 = " billion years with a standard deviation of ",
  quest.txt3 = " billion years. A sample from the second cluster has an average age of ",
  quest.txt4 = " billion years with a standard deviation of ",
  quest.txt5 = " billion years. NASA samples ",
  quest.txt6 = " stars from the first cluster, and ",
  quest.txt7 = " stars from the second cluster. What is the 95% CI for the difference between mean ages of the first cluster vs. the second cluster?", # The above 7 question texts are static texts for the full question
  dat.size = 1, # This is the number of values to be randomly generated for the dataset
  digits = 2, # This is the number of decimal places to round off the data
  loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "Is this a 1 or 2 sample mean CI? Are the groups independent or not? Pick the closest answer.", # This is a student hint, visible to them during the exam on e-learning
  feedback = "Did you use xbar1 - xbar2 +/- 1.96*sqrt((s1)^2/(n1) + (s2)^2/(n2))?" # This is student feedback, visible after the exam
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
    data1 <- sample(seq(2, 7, 10^-digits), size = 1) # randomly generating data of size 1 for sample mean of sample 1
    data2 <- sample(seq(.5, 3, 10^-digits), size = 1) # randomly generating data of size 1 for sample standard deviation of sample 1
    data3 <- sample(seq(1, 6, 10^-digits), size = 1) # randomly generating data of size 1 for sample mean of sample 2
    data4 <- sample(seq(.5, 3, 10^-digits), size = 1) # randomly generating data of size 1 for sample standard deviation of sample 2
    data5 <- sample(100:200, size = 1) # randomly generating sample size of sample 1 
    data6 <- sample(100:200, size = 1) # randomly generating sample size of sample 2
    lb <- round(data1 - data3 - 1.96*sqrt(data2^2/data5 + data4^2/data6),
                digits = digits) # creating lower bound of the 95% confidence interval
    ub <- round(data1 - data3 + 1.96*sqrt(data2^2/data5 + data4^2/data6),
                digits = digits) # creating upper bound of the 95% confidence interval
    corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "") # this is the correct answer to the question
    up.min <- round(ub + .1, digits) # This is the minimum value for incorrect answers above the correct answer
    down.max <- round(lb - .1, digits) # This is the maximum value for incorrect answers below the correct answer
    if(lb <= -4){
      seq1 <- round(seq(up.min, up.min + 3, 10^-digits), digits) # generating minimum values for incorrect answers above the correct answer within a certain limit
      mat1 <- expand.grid(seq1,seq1) # creating a data frame from the combinations of minimum values for incorrect answers above the correct answer within a certain limit
      mat1 <- mat1[mat1[,1] < mat1[,2], ]
      ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]} # randomly generated incorrect answers given the condition of lower bound
    else{if(ub >= 6){
      seq2 <- round(seq(down.max - 3, down.max, 10^-digits), digits) # generating maximum values for incorrect answers below the correct answer within a certain limit
      mat2 <- expand.grid(seq2,seq2) # creating a data frame from the combinations of maximum values for incorrect answers below the correct answer within a certain limit
      mat2 <- mat2[mat2[,1] < mat2[,2], ]
      ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]} # randomly generated incorrect answers given the condition of upper bound
      else{
        seq3 <- round(c(seq(down.max - 3, down.max, 10^-digits), seq(up.min, up.min + 3, 10^-digits)), digits) # generating minimum and maximum values for incorrect answers within a certain limit
        mat3 <- expand.grid(seq3,seq3) # creating a data frame from the combinations of minimum and maximum values for incorrect answers within a certain limit
        mat3 <- mat3[mat3[,1] < mat3[,2], ]
        ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
    ans.txt <- c()
    for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")} # These are randomly generated incorrect answers.
    content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                     data2, quest.txt3, data3, quest.txt4,
                                     data4, quest.txt5, data5, quest.txt6,
                                     data6, quest.txt7,
                                     collapse = "", sep= ""),
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

twomeanCIMC1() # creating the csv file
