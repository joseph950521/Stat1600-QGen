##### SkewGraphMC1 #####
SkewGraphMC1= function(
title = "SkewGraphMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt = "While analyzing a dataset, a researcher plots a histogram of one of her variables. The histogram she makes is depicted. What type of skewness, if any, is present in the variable's distribution?", # The above question text is the static text for the full question
dat.size = 10000, # This is the number of values to be randomly generated for the dataset
digits = 3, # This is the number of decimal places to round off the data
loc.path = "/Users/josephyang/Desktop/School Stuff/STAT 1600/Course Development/Question Generators/Stat1600-QGen/SkewGraphMC1 images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "Focus on the tails.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Left Skew: Left tail. Right Skew: Right tail. No Skew: Symmetric tails."
) # This is student feedback, visible after the exam
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 7 + which.max(points) # This is the row index of the correct answer
  left.shape1 <- runif(1,40,50) # creating the first shape parameter of a beta distribution
  left.shape2 <- runif(1,1,10) # creating the second shape parameter of a beta distribution
  left.data <- rbeta(dat.size, left.shape1, left.shape2) # creating a beta distribution data that has left skewed tail
  right.shape1 <- runif(1,1,10) # creating the first shape parameter of a beta distribution
  right.shape2 <- runif(1,40,50) # creating the second shape parameter of a beta distribution
  right.data <- rbeta(dat.size, right.shape1, right.shape2) # creating a beta distribution data that has right skewed tail
  sym.data <- rnorm(dat.size, 0, 1) # creating a normal symmetric distribution data
  data.dec <- sample(c(1,2,3), size = 1) # creating a vector for taking decision to use the a specific kind of data
  data <- if(data.dec == 1){sym.data}else{if(data.dec == 2){left.data}else{right.data}} # creating a dataset based on a random decision value
  corr.ans <- if(identical(data, sym.data)){"No Skew"}else{if(identical(data, right.data)){"Right Skew"}else{"Left Skew"}} # This is the correct answer to the question
  ans.txt <- if(corr.ans == "No Skew"){c(sample(c("Right Skew", "Left Skew", "Curve Skew", "Histographic Skew"), size = answers-1),
                                         sample(c("All of These", "None of These"), size = 1))}
  else{if(corr.ans == "Left Skew"){c(sample(c("Right Skew", "No Skew", "Curve Skew", "Histographic Skew"), size = answers-1),
                                     sample(c("All of These", "None of These"), size = 1))}
    else{c(sample(c("No Skew", "Left Skew", "Curve Skew", "Histographic Skew"), size = answers-1),
           sample(c("All of These", "None of These"), size = 1))}} # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, quest.txt, points.per.q, difficulty,
               paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = "")) # creating the image files in the designated place
  hist(data, xlim=c(min(data),max(data)), probability=T, 
       col='lightblue', xlab=' ', ylab=' ', axes=F,
       main = "Researcher's Histogram") # Creating a histogram graph from one of the three datasets
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

SkewGraphMC1() # creating the csv file