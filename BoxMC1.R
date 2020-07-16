##### BoxMC1 #####
library(graphics) # the pacakge needed for this particular question to create generic boxplots
BoxMC1= function(
title = "BoxMC1", # Question-bank title that will be easily viewable in e-learning
  n = 200, # Number of questions to generate
  type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "While analyzing a dataset, a researcher makes a boxplot of one of her variables. The boxplot is depicted above. What is the value of the ",
quest.txt2 = "?", # The above 2 question texts are static text for the full question
dat.size = 41, # This is the number of values to be randomly generated for the dataset
digits = 0, # This is the number of decimal places to round off the data
loc.path = "Images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "You can read about boxplots in the data presentation chapter of your coursepack.", # This is a student hint, visible to them during the exam on e-learning
feedback = "From lowest to highest, boxplots have lines at the minimum, Q1, Q2 (median), Q3, and the max." # This is student feedback, visible after the exam
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
  left.data <- c(sample(seq(100, 199, by = 10^(-digits)), size = ceiling(dat.size/6)),
                 sample(seq(200, 299, by = 10^(-digits)), size = ceiling(2*dat.size/6)),
                 sample(seq(300, 400, by = 10^(-digits)), size = ceiling(3*dat.size/6))) # randomly generating a dataset that is left skewed
  right.data <- c(sample(seq(300, 400, by = 10^(-digits)), size = ceiling(dat.size/6)),
                  sample(seq(200, 299, by = 10^(-digits)), size = ceiling(2*dat.size/6)),
                  sample(seq(100, 199, by = 10^(-digits)), size = ceiling(3*dat.size/6))) # randomly generating a dataset that is right skewed
  sym.data <- c(sample(seq(300, 400, by = 10^(-digits)), size = ceiling(1.5*dat.size/6)),
                sample(seq(200, 299, by = 10^(-digits)), size = ceiling(3*dat.size/6)),
                sample(seq(100, 199, by = 10^(-digits)), size = ceiling(1.5*dat.size/6))) # randomly generating a dataset that is symmetric in both sides
  data.dec <- sample(c(1,2,3), size = 1) # creating a vector for taking decision to use the a specific kind of data
  data <- sample(round(if(data.dec == 1){sym.data}
                       else{if(data.dec == 2){left.data}
                         else{right.data}},
                       digits = digits),
                 size = dat.size) # creating a dataset based on a random decision value
  data1 <- sample(c("minimum", "first quartile", "second quartile", "third quartile",
                    "maximum"), size = 1) # these are the options created for the question
  corr.ans <- if(data1 == "minimum"){min(data)}
  else{if(data1 == "first quartile"){fivenum(data)[2]}
    else{if(data1 == "second quartile"){fivenum(data)[3]}
      else{if(data1 == "third quartile"){fivenum(data)[4]}
        else{if(data1 == "maximum"){max(data)}}}}} # This is the correct answer to the question
  ans.txt <- if(corr.ans == min(data)){sample(sort(data)[4:dat.size], size = answers)}
  else{if(corr.ans == fivenum(data)[2]){sample(c(sort(data)[1:floor(dat.size/8)],
                                                 sort(data)[ceiling(3*dat.size/8):dat.size]),
                                               size = answers)}
    else{if(corr.ans == fivenum(data)[3]){sample(c(sort(data)[1:floor(3*dat.size/8)],
                                                   sort(data)[ceiling(5*dat.size/8):dat.size]),
                                                 size = answers)}
      else{if(corr.ans == fivenum(data)[4]){sample(c(sort(data)[1:floor(5*dat.size/8)],
                                                     sort(data)[ceiling(7*dat.size/8):dat.size]),
                                                   size = answers)}
        else{if(corr.ans == max(data)){sample(sort(data)[1:(dat.size-3)],
                                              size = answers)}}}}} # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = "")) # creating the image files in the designated place
  boxplot(data, horizontal = T, col = 'lightblue') # creating the question specific boxplot
  title("Researcher's Boxplot") # creating a title for each boxplot
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

BoxMC1() # creating the csv file