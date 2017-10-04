sailors <- data.frame(tid = c(rep(1:2, each = 5),2), 
		sid = c(rep(seq(5), 2),11),
		sname = c("John", "Adam", "Sam", "Mary", "Eve",
                          "Mallory", "Pete", "Rose", "Carol", "Veronica","Anne"), 
		age = sample(20:25, 11, replace = TRUE))
boats <- data.frame(bid = 100 + 1:5, 
		bname = c("Fortune", "Luck", "Opportunity", "Fate", "Chance"), 
		color = rep(c("red", "blue"), c(2, 3)))
reserves <- data.frame(tid = sample(1:2, 10, replace = TRUE), 
		sid = sample(1:5, 10, replace = TRUE), 
		bid = sample(100 + 1:5, 10, replace = TRUE), 
		day = sample(1:365, 10, replace = TRUE))
sailors$sname <- as.character(sailors$sname)
boats$sname <- as.character(boats$bname)
boats$color <- as.character(boats$color)
