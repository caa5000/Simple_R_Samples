#Christian Acosta Homework Week 11

#Use scan() to read Amazon reviews data set in. 
amazonReviews <- scan("C:/Data Science Masters Program/DS 710 - Programming for Data Science/AmazonReview.csv", what = character(), sep = ",", skip = 1, nlines = 100000)

summary(amazonReviews)
head(amazonReviews)
amazonReviews_matrix = t(matrix(amazonReviews, nrow = 7))
amazonReviews_matrix
head(amazonReviews_matrix) #verified header didn't carry over.
amazonReviews_header <-  scan("C:/Data Science Masters Program/DS 710 - Programming for Data Science/AmazonReview.csv", what = character(), sep = ",", nlines = 1)
colnames(amazonReviews_matrix) = amazonReviews_header #Assigns header to matrix.
head(amazonReviews_matrix) #verified header now attached. 

#Create new vars by converting total votes, review length, number ! pts, helpful fraction into numeric vectors.
numHelpful = as.numeric(amazonReviews_matrix[,1])
totalVotes = as.numeric(amazonReviews_matrix[,3])
reviewLength = as.numeric(amazonReviews_matrix[,5])
excPoints = as.numeric(amazonReviews_matrix[,6])
helpFraction = as.numeric(amazonReviews_matrix[,7])

#Review helpFraction data for unrealistic data. 
#UNSURE IF THIS CHANGE WAS NEEDED IN MATRIX OR JUST IN VECTORS? 
#Since it is a fraction, anything over 1 is not correct.
summary(helpFraction) 
#If helpFraction > 1, set helpFraction, Helpful & Rated Reviews to NA.
which(helpFraction>1) #44,737, 64442 need to be set to NA
helpFraction[64422] <- NA
helpFraction[44737] <- NA
numHelpful[64422] <- NA
numHelpful[44737] <- NA
totalVotes[64422] <- NA
totalVotes[44737] <- NA
#totalVotes[44737] #Verified.
#amazonReviews_matrix[64422,7] #This is an example of what may need to be changed. 

#Create a new variable that describes whether more than 50% of the people who voted
#considered it helpful. Call it helpful reviews. 
#Look through the helpFraction and pluck out all with greater than .5 ratio.
HelpfulReviews <- as.logical(helpFraction>.5)
HelpfulReviews[26348] #Verified it is leaving out blanks and anything that IS .5 . 
length(HelpfulReviews) #36729 helpful reviews. 

#Are helpful reviews longer than unhelpful ones?
#Not needed after changing to as.logical up above. 
#unhelpfulReviews <- which(helpFraction<=.5)
#is.na(unhelpfulReviews)
#helpFraction[92993] #Tested a couple random ones to make sure.
#length(unhelpfulReviews) #6634 records are unhelpful
#HelpfulReviews contains + ones, unhelpfulReviews contain <.5 ones.
mean(reviewLength[which(HelpfulReviews==TRUE)]) #502.77 @ 3,6729 
mean(reviewLength[which(HelpfulReviews==FALSE)]) #497.29 @ 6,634 
#mean(reviewLength[which(unhelpfulReviews==TRUE)])
hist(reviewLength) #very much right skewed, apply log transformation.
logReviewLength <- log10(reviewLength)
hist(logReviewLength)

tHelpfulReview <- logReviewLength[which(HelpfulReviews==TRUE)]
tUnhelpfulReview <- logReviewLength[which(HelpfulReviews==FALSE)]
t.test(tHelpfulReview, tUnhelpfulReview, alternative="greater")

#Do products with more reviews tend to have more votes on their reviews?

#Use tapply to find the maximum number of votes received by any of the product's reviews. 
?tapply #(batting average, by team, apply function)
#Not sure how to do it with a subgroup. 
#max(totalVotes, na.rm=TRUE) #539 is the max.
maxprodIDVotes <- tapply(totalVotes, Product.ID, FUN = max ,na.rm=TRUE) #max total votes by prodID
countProdVotes <- tapply(totalVotes, Product.ID, length)
countProdVotes
max(prodIDVotes) #539 max
length(countProdVotes) #11760 GOOD
length(prodIDVotes) #11,760
length(unique(Product.ID)) #11760 GOOD.

#Make a scatterplot of max # votes as a function of # of reviews.
plot(countProdVotes, maxprodIDVotes) 
#looks like there may be a linear trend, but i think a long transformation is in order.

plot(logCountProdVotes, maxprodIDVotes)
#countProdVotes[which(is.nan(countProdVotes))] <- NA
#countProdVotes[which(countProdVotes==Inf)] <- NA
#maxprodIDVotes[which(is.nan(countProdVotes))] <- NA
#maxprodIDVotes[which(countProdVotes==Inf)] <- NA
#which(is.na(maxprodIDVotes))
#modelCountMaxVotes <- lm(maxprodIDVotes ~ countProdVotes, na.action=na.exclude)
#modelCountMaxVotes
?plot

#Create subsets of max votes and number reviews to products with 1 or more votes.
#Create a subset that can be used to make sure each prod has at least one vote. 
someVoting <- which(maxprodIDVotes>=1)
someVoting 
maxprodIDVotes <- maxprodIDVotes[someVoting]
countProdVotes <- countProdVotes[someVoting]
plot(countProdVotes, maxprodIDVotes)

logMaxProdIDVotes <- log(maxprodIDVotes)
logCountProdVotes <- log(countProdVotes)

plot(logCountProdVotes, logMaxProdIDVotes)

modelLogReviews <- lm(logMaxProdIDVotes ~ logCountProdVotes)
modelLogReviews
summary(modelLogReviews)
abline(modelLogReviews)
#.5906x + .434707
#modelCountMaxVotes <- lm(maxprodIDVotes ~ countProdVotes, na.action=na.exclude)

#------------ UNUSED CODE -----------------#

#length(HelpfulReviews)
#length(reviewLength)
#mean(reviewLength) #Mean 441.6 checks out with CSV file



#amazonReviews_df = as.data.frame(amazonReviews)
#summary(amazonReviews_df)
#attach(amazonReviews_df)
#rm(Helpful.Fraction)
#Helpful.Fraction
head(amazonReviews_df)
#amazonReviews <- scan("C:/Data Science Masters Program/DS 710 - Programming for Data Science/AmazonReview.csv", what = list(Helpful = numeric(),Product.ID = character(),Rated.Reviews = numeric(),Review.Score = numeric(),Review.Length = numeric(),Exclamation.Points = numeric(),Helpful.Fraction = numeric()), sep = ",", skip = 1, nlines = 100000 )
#Convert into a matrix. each row representes one review.
#Read header row into R. Use it to create column names for the matrix.


amazonReviews_matrix[6,4]
