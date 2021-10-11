# Predictive Ws

This project attempts to predict students that completely withdraw from their entire course load after the semester has started. 

Overall, this project had minimal success, but has potential to be improved, and has been (and continues to be) a great learning experience. 

Multiple models were tested: Naive Bayes, random forest, logistic regression, and decision trees. 

A variety of factors, such as the highly imbalanced data set with regard to the binary response variable, as well as the large amount of categorical data, make this prediction a challenge. Data were oversampled, undersampled, and random over-sampled (ROSE) as test methods to compensate for the imbalance, to minimal success. 

However, there is a lot of room for continued experimentation: 
- Simplifying and/or bucketing particular features
- Splitting the data set into specific groups and building group-specific models (perhaps based on class level, or part-time/full-time students, etc.)
- Subset the data based on further specifics related to the withdrawals
    - The timing of the withdrawals
    - The time elapsed from the start of the semester to the first W
    - The time elapsed between the first W and the last W
    - etc. 
- Perhaps changing the response variable to "at least one W" versus all Ws
- Or limiting the data set to **only** people who have at least one W

Long story short, there are lots of improvements to be made, but I also acknowledge that students withdraw all the time for all types of reasons, and this just may be an inherently difficult thing to predict.

[Note: Data sets are at the student level, and contain sensitive information, and have thus not been provided.]
