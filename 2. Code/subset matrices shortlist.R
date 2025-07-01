#Since the dataset is too large for calculating nestedness contributions, we make it smaller in two steps.
#First, we create a subset containing only the 10% of largest collections. 
dataset_top = subset(dataset, user_collection_size > quantile(user_collection_size, 0.9))
barplot(sort(dataset_top$user_collection_size, decreasing=T),
        main="top reviewers")
#Second, from this dataset of largest collections, we subset the 70% of most frequently rated films.
dataset_top = subset(dataset_top, film_frequency > quantile(film_frequency, 0.3))
barplot(sort(dataset_top$film_frequency, decreasing=T),
        main="top items")

#Making this new subset our new main working dataset
dataset = dataset_top