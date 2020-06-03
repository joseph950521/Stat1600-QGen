##### 2PropCIMC1 #####
twopropCIMC1=function(
  title = "2PropCIMC1", # Question-bank title that will be easily viewable in e-learning
  n = 200, # Number of questions to generate
  type = "MC", # The question type, one of many possible types on e-learning
  answers = 5, # Number of answers per MC question
  points.per.q = 4, # Number of points the question is worth (on the test)
  difficulty = 1, # An easily viewable difficulty level on e-learning
  quest.txt1 = "A new pesticide is tested on a group of crop-destroying beetles. The sample data shows that ",
  quest.txt2 = " of this first group dies as a result. A second group of beetles is dosed with a standard pesticide, and ",
  quest.txt3 = " of this second group dies as a result. ",
  quest.txt4 = " beetles are in the first test-pesticide group and ",
  quest.txt5 = " beetles are in the second standard-pesticide group. What is the 95% CI for the difference between proportions in the test group vs. the standard group?", # The above 5 question texts are static text for the full question
  dat.size = 1, # This is the number of values to be randomly generated for the dataset
  digits = 2, # This is the number of decimal places to round off the data
  loc.path, # This is the local path used to store any randomly generated image files
  e.path, # This is the path on e-learning used to store any above-implemented image files
  hint = "You'll need to calculate a confidence interval for the difference between 2 independent proportions. Pick the closest answer.", # This is a student hint, visible to them during the exam on e-learning
  feedback = "Did you use phat1 - phat2 - 1.96*sqrt(phat1*qhat1/n1 + phat2*qhat2/n2), and phat1 - phat2 + 1.96*sqrt(phat*qhat/n1 + phat2*qhat2/n2)?" # This is student feedback, visible after the exam
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
    data1 <- sample(seq(.65, .8, 10^-digits), size = 1) #randomly generating sample proportion for first sample
    data2 <- sample(seq(.3,.55, 10^-digits), size = 1) #randomly generating sample proportion for second sample
    data3 <- sample(100:400, size = 1) #randomly generating sample size for first sample
    data4 <- sample(100:400, size = 1) #randomly generating sample size for second sample
    lb <- round(data1 - data2 - 1.96*sqrt(data1*(1-data1)/data3 + data2*(1-data2)/data4),
                digits = digits) # This is how we're randomly generating 'lower bounds' data for this question
    ub <- round(data1 - data2 + 1.96*sqrt(data1*(1-data1)/data3 + data2*(1-data2)/data4),
                digits = digits) # This is how we're randomly generating 'upper bounds' data for this question
    corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "") # This is the correct answer to the question
    up.min <- round(ub + .05, digits) # This is the minimum value for incorrect answers above the correct answer
    down.max <- round(lb - .05, digits) # This is the maximum value for incorrect answers below the correct answer
    if(lb <= .05){
      seq1 <- seq(up.min, 1 + 10^-digits, 10^-digits)
      mat1 <- expand.grid(seq1,seq1)
      mat1 <- mat1[mat1[,1] < mat1[,2], ]
      ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]} # This is the partial part of randomly generated incorrect answers based on lower bound value 
    else{if(ub >= .95){
      seq2 <- seq(0 - 10^-digits, down.max, 10^-digits)
      mat2 <- expand.grid(seq2,seq2)
      mat2 <- mat2[mat2[,1] < mat2[,2], ]
      ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]} # This is the partial part of randomly generated incorrect answers based on upper bound value 
      else{
        seq3 <- c(seq(0 - 10^-digits, down.max, 10^-digits), seq(up.min, 1 + 10^-digits, 10^-digits))
        mat3 <- expand.grid(seq3,seq3)
        mat3 <- mat3[mat3[,1] < mat3[,2], ]
        ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}} # This is the partial part of randomly generated incorrect answers based on middle 95% confidence interval 
    ans.txt <- c()
    for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")} # These are randomly generated incorrect answers
    content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                     data2, quest.txt3, data3, quest.txt4,
                                     data4, quest.txt5,
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

twopropCIMC1() # creating the csv file
