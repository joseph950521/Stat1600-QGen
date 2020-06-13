##### RegCIMC1 #####
RegCIMC1= function(
title = "RegCIMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "NASA compares the ages of stars (in billions of years) to their surface temperatures (in thousands of degrees Kelvin). A sample of ",
quest.txt2 = " stars has a standard deviation in age of ",
quest.txt3 = " billion years, and a standard deviation in temperature of ",
quest.txt4 = " thousands of degrees. The sample's age and temperature have a correlation coefficient of ",
quest.txt5 = ". What is the 95% CI for the slope of the regression line temperature (y) vs. age (x)?", # The above 5 question texts are static text for the full question
dat.size = 1, # this is the number of values to be generated randomly for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "You need to use the given statistics to find slope, standard error, and the lower and upper bounds.",  # This is a student hint, visible to them during the exam on e-learning
feedback = "Did you use r*sy/sx +/- 1.96*sqrt((1-r^2)/(n-2))*(sy/sx)?" # This is student feedback, visible after the exam
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
  data1 <- sample(50:150, size = 1) # randomly generating sample size
  data2 <- sample(seq(2, 4, 10^-digits), size = 1) # randomly generating standard deviation of age
  data3 <- sample(seq(5, 7, 10^-digits), size = 1) # randomly generating standard deviation of temperature
  data4 <- sample(seq(-1, 1, 10^-digits), size = 1) # randomly generating correlation value
  lb <- round(data4*data3/data2 - 1.96*sqrt((1-data4^2)/(data1-2))*(data3/data2),
              digits = digits) # creating lower bound of the 95% confidence interval
  ub <- round(data4*data3/data2 + 1.96*sqrt((1-data4^2)/(data1-2))*(data3/data2),
              digits = digits) # creating upper bound of the 95% confidence interval
  corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "") # this is the correct answer to the question
  up.min <- round(ub + .1, digits) # This is the minimum value for incorrect answers above the correct answer
  down.max <- round(lb - .1, digits) # This is the maximum value for incorrect answers below the correct answer
  if(lb <= -1.9){
    seq1 <- round(seq(up.min, up.min + 2, 10^-digits), digits) # generating minimum values for incorrect answers above the correct answer within a certain limit
    mat1 <- expand.grid(seq1,seq1) # creating a data frame from the combinations of minimum values for incorrect answers above the correct answer within a certain limit
    mat1 <- mat1[mat1[,1] < mat1[,2], ]
    ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
  else{if(ub >= 1.9){
    seq2 <- round(seq(down.max - 2, down.max, 10^-digits), digits) # generating maximum values for incorrect answers below the correct answer within a certain limit
    mat2 <- expand.grid(seq2,seq2) # creating a data frame from the combinations of maximum values for incorrect answers below the correct answer within a certain limit
    mat2 <- mat2[mat2[,1] < mat2[,2], ]
    ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
    else{
      seq3 <- round(c(seq(down.max - 2, down.max, 10^-digits), seq(up.min, up.min + 2, 10^-digits)), digits) # generating minimum and maximum values for incorrect answers within a certain limit
      mat3 <- expand.grid(seq3,seq3) # creating a data frame from the combinations of minimum and maximum values for incorrect answers within a certain limit
      mat3 <- mat3[mat3[,1] < mat3[,2], ]
      ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
  ans.txt <- c()
  for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")}  # These are randomly generated incorrect answers based on the condition
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
                                   collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",6), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param  # This is indexing and storing all the row names
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
}
questions <- questions[(9+answers):((8+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

RegCIMC1() # creating the csv file