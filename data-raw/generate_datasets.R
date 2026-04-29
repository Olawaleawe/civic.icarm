
set.seed(2025)
n <- 1000L
age <- sample(18L:85L, n, replace=TRUE)
education <- ordered(sample(c("primary","secondary","vocational","university","postgraduate"), n, replace=TRUE, prob=c(0.05,0.30,0.20,0.30,0.15)), levels=c("primary","secondary","vocational","university","postgraduate"))
income <- round(rnorm(n,0,1.5),2)
pol_int <- sample(1L:5L, n, replace=TRUE, prob=c(0.10,0.15,0.30,0.25,0.20))
news <- pmax(0L, round(rnorm(n,5,4)))
civic_org <- rbinom(n,1L,0.35)==1L
region <- factor(sample(c("Baden-Wurttemberg","Bavaria","Berlin","Brandenburg","North-Rhine-Westphalia","Saxony","Hamburg"), n, replace=TRUE))
gender <- factor(sample(c("female","male","diverse"), n, replace=TRUE, prob=c(0.49,0.49,0.02)))
mig_bg <- rbinom(n,1L,0.25)==1L
logit_p <- -1.5+0.03*(age-40)+0.40*as.numeric(education)+0.20*income+0.35*pol_int+0.10*news+0.50*civic_org-0.10*(gender=="diverse")-0.08*mig_bg+rnorm(n,0,0.5)
p_vote <- 1/(1+exp(-logit_p))
voted <- factor(ifelse(rbinom(n,1,p_vote)==1,"yes","no"))
civic_voting <- tibble::tibble(voted, age, education, income, political_interest=pol_int, news_consumption=news, civic_org_member=civic_org, region, gender, migration_background=mig_bg)

n2 <- 800L
age2 <- sample(18L:45L, n2, replace=TRUE)
edu2 <- factor(sample(c("Abitur","Bachelor","Master","Other"), n2, replace=TRUE, prob=c(0.20,0.45,0.30,0.05)))
news2 <- pmax(0L, round(rnorm(n2,4,3)))
stats_c <- rbinom(n2,1L,0.55)==1L
subj <- factor(sample(c("Social Studies","Geography","History","Ethics","German","Other"), n2, replace=TRUE))
gender2 <- factor(sample(c("female","male","diverse"), n2, replace=TRUE, prob=c(0.65,0.33,0.02)))
mig2 <- rbinom(n2,1L,0.20)==1L
civic_score <- round(pmin(100,pmax(0,40+0.5*(age2-25)+8*(edu2=="Master")+4*(edu2=="Bachelor")+5*stats_c+2*news2-3*mig2+rnorm(n2,0,12))))
data_lit_score <- round(pmin(100,pmax(0,35+6*stats_c+3*news2+4*(edu2 %in% c("Master","Bachelor"))+rnorm(n2,0,14))))
civic_education <- tibble::tibble(civic_knowledge_score=civic_score, data_literacy_score=data_lit_score, age=age2, education=edu2, news_consumption=news2, stats_course=stats_c, teaching_subject=subj, gender=gender2, migration_background=mig2)

n3 <- 1000L
set.seed(2025)
duration <- sample(6L:72L,n3,TRUE)
credit_amount <- round(runif(n3,500,15000))
age3 <- sample(18L:75L,n3,TRUE)
housing <- factor(sample(c("own","rent","free"),n3,TRUE,prob=c(0.50,0.35,0.15)))
job <- factor(sample(c("skilled","unskilled","management","other"),n3,TRUE,prob=c(0.50,0.20,0.20,0.10)))
gender_proxy <- factor(sample(c("male","female"),n3,TRUE,prob=c(0.60,0.40)))
purpose <- factor(sample(c("car","education","furniture","other"),n3,TRUE,prob=c(0.30,0.15,0.25,0.30)))
logit_cr <- 1.0-0.01*duration-0.00005*credit_amount+0.01*(age3-40)+0.30*(housing=="own")-0.20*(job=="unskilled")+0.10*(gender_proxy=="male")+rnorm(n3,0,0.8)
p_good <- 1/(1+exp(-logit_cr))
credit_risk <- factor(ifelse(rbinom(n3,1,p_good)==1,"good","bad"))
civic_german_credit <- tibble::tibble(credit_risk, duration, credit_amount, age=age3, housing, job, gender_proxy, purpose)

dir.create("data", showWarnings=FALSE)
save(civic_voting, file="data/civic_voting.rda", compress="bzip2")
save(civic_education, file="data/civic_education.rda", compress="bzip2")
save(civic_german_credit, file="data/civic_german_credit.rda", compress="bzip2")
message("Done: ", nrow(civic_voting), " / ", nrow(civic_education), " / ", nrow(civic_german_credit))
