dataScience <- findJobs("https://www.indeed.com/jobs?q=\"data+scientist\"&l=Seattle%2C+WA",1)

businessAnalyst <- findJobs("https://www.indeed.com/jobs?q=\"business analyst\"&l=Seattle,+WA",0)

businessIntAnalyst <- findJobs("https://www.indeed.com/jobs?q=\"business intelligence analyst\"&l=Seattle,+WA",0)

dataAnalyst <- findJobs("https://www.indeed.com/jobs?q=\"data analyst\"&l=Seattle,+WA",0)

save(dataScience, businessAnalyst, businessIntAnalyst, dataAnalyst, file = "seattleData.RData")

