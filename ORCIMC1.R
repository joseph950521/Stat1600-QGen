##### ORCIMC1 #####
library(gridExtra) # the pacakge needed for this particular question to create multiple grid-level plots in a page
ORCIMC1= function(
  title = "ORCIMC1", # Question-bank title that will be easily viewable in e-learning
  n = 200, # Number of questions to generate
  type = "MC", # The question type, one of many possible types on e-learning
  answers = 5, # Number of answers per MC question
  points.per.q = 4, # Number of points the question is worth (on the test)
  difficulty = 1, # An easily viewable difficulty level on e-learning
  quest.txt1 = "Both a new medication and placebo were administered to two separate groups of subjects. The subjects were monitored for recovery from an illness and the results were recorded in the 2x2 contingency table above. What is the 95% CI for the odds ratio given recovery in subjects who received the medication vs. those who received the placebo?", # The above text is the static text for the question
  Treatments = c("Medication", "Placebo"), #creating a vector of the treatment characters in the question
  digits = 2, # This is the number of decimal places to round off the data
  loc.path = "/Users/josephyang/Desktop/School Stuff/STAT 1600/Course Development/Question Generators/Stat1600-QGen/ORCIMC1 images/", # This is the local path used to store any randomly generated image files
  e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
  hint = "You can follow your a, b, c, d formula given in lectures and the coursepack.", # This is a student hint, visible to them during the exam on e-learning
  feedback = "OR = ad/(bc). SE = sqrt(1/a + 1/b + 1/c + 1/d). ME for ln(OR) = 1.96*SE." # This is student feedback, visible after the exam
)
{
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
             rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
  questions <- data.frame() # This opens a data frame to store the randomly generated questions below
  for(i in 1:n)
  {
    ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
    points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
    corr.ind <- 7 + which.max(points) # This is the row index of the correct answer
    Recovered <- c(sample(50:100, size = 1), sample(25:75, size = 1))
    Unrecovered <- sample(100:200, size = 2)
    data <- data.frame(Treatments, Recovered, Unrecovered, stringsAsFactors = FALSE) # This is how we're randomly generating data frame for this question
    lb <- round(exp(log(data[1,2]*data[2,3]/(data[1,3]*data[2,2])) - 1.96*sqrt(1/data[1,2] + 1/data[2,2] + 1/data[1,3] + 1/data[2,3])),
                digits = digits) # creating lower bound of the 95% confidence interval
    ub <- round(exp(log(data[1,2]*data[2,3]/(data[1,3]*data[2,2])) + 1.96*sqrt(1/data[1,2] + 1/data[2,2] + 1/data[1,3] + 1/data[2,3])),
                digits = digits) # creating upper bound of the 95% confidence interval
    corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "") # this is the correct answer to the question
    up.min <- round(ub + .08, digits) # This is the minimum value for incorrect answers above the correct answer
    down.max <- round(lb - .08, digits) # This is the maximum value for incorrect answers below the correct answer
    if(lb <= .4){
      seq1 <- seq(up.min, 5 + 10^-digits, 10^-digits) # generating minimum values for incorrect answers above the correct answer within a certain limit
      mat1 <- expand.grid(seq1,seq1) # creating a data frame from the combinations of minimum values for incorrect answers above the correct answer within a certain limit
      mat1 <- mat1[mat1[,1] < mat1[,2], ]
      ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
    else{if(ub >= 3.2){
      seq2 <- seq(-0.1 - 10^-digits, down.max, 10^-digits) # generating maximum values for incorrect answers below the correct answer within a certain limit
      mat2 <- expand.grid(seq2,seq2) # creating a data frame from the combinations of maximum values for incorrect answers below the correct answer within a certain limit
      mat2 <- mat2[mat2[,1] < mat2[,2], ]
      ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
      else{
        seq3 <- c(seq(-0.1 - 10^-digits, down.max, 10^-digits), seq(up.min, 5 + 10^-digits, 10^-digits)) # generating minimum and maximum values for incorrect answers within a certain limit
        mat3 <- expand.grid(seq3,seq3) # creating a data frame from the combinations of minimum and maximum values for incorrect answers within a certain limit
        mat3 <- mat3[mat3[,1] < mat3[,2], ]
        ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
    ans.txt <- c()
    for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")} # These are randomly generated incorrect answers for different limit condition
    content <- c(type, ID, ID, quest.txt1,
                 points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
                 points, hint, feedback) # This is collecting a lot of the above information into a single vector
    options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
    options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
    questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
    questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
    questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
    jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
         height = 30*nrow(data), width = 81*ncol(data)) # creating the image files in the designated place
    p <- tableGrob(data) # Creating the summary statistics for the graphical item
    grid.arrange(p) # arranging all the summary statistics for the graphical items in one location
    dev.off()
  }
  questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
  write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
              row.names=F, col.names=F) # Writing the CSV file
}

ORCIMC1() # creating the csv file
