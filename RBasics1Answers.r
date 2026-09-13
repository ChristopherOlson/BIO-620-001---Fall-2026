12 %% 5

x <- 1:10
y <- 1/x
plot(x,y)

y = c(12,4,12,5,27,23) 
y = c(y,y) 
y 
sum(y)
mean(y)
sd(y)
var(y)
median(y)
max(y)
min(y)
summary(y)

x <- 1:50
z <- x*2
sum(z)
mean(z)
sd(z)
var(z)
median(z)
max(z)
min(z)
quantile(z,0.25)
quantile(z,0.75)


y <- c(8,3,5,7,6,6,8,9,2,3,9,4,10,4,11)

even <- y %% 2 == 0
odd <- y %% 2 != 0

even
odd

which(even)
which(odd)


400 / 17

12 * ((6 * 15) / (40 / 21))

250^(1/3)

log10(1000)


seq(0, 16, by = 4)

seq(0.3, 1.5, by = 0.3)

seq(0, -40, by = -10)

rep(c("tropics", "temperate", "boreal"), each = 3)


x <- c(5,3,8,2,9,3,6,9,1,0,2,7)

sum(x)
mean(x)
length(x)


x <- c(3,9,6,1,9,4,7,8,2,6,3,8,0,2,5)

sort(x)

rev(sort(x))


apes <- read.csv("orangutan.csv")

names(apes)

str(apes)


apes[apes$location == "Borneo", ]


males <- apes[apes$sex == "male", ]

males <- males[rev(order(males$weight)), ]

males


min(apes$weight)


females <- apes[apes$sex == "female", ]

range(females$weight)


mean(apes$weight[apes$location == "Sumatra"])


sum(apes$tool_use == "yes")


female_weights <- sort(females$weight, decreasing = TRUE)

sum(female_weights[1:3])


organisms <- c("lion",
               "tiger",
               "elephant",
               "giraffe",
               "zebra",
               "gorilla",
               "chimpanzee",
               "orangutan",
               "rhinoceros",
               "hippopotamus")

sample(organisms, size = 10, replace = TRUE)

sample(organisms, size = 10, replace = TRUE)

sample(organisms, size = 10, replace = FALSE)

sample(organisms, size = 10, replace = FALSE)


baseball <- read.csv("baseball.csv")

names(baseball)

str(baseball)


sum(baseball$games)

sum(baseball$wins)

sum(baseball$losses)


baseball$difference <- baseball$wins - baseball$losses

max(baseball$difference)

min(baseball$difference)


baseball[baseball$difference == max(baseball$difference), ]

baseball[baseball$difference == min(baseball$difference), ]


baseball$win_percentage <- baseball$wins / baseball$games

baseball[order(baseball$win_percentage, decreasing = TRUE), ][1:3, ]

baseball[order(baseball$win_percentage), ][1:3, ]


baseball$runs_ratio <- baseball$runs_allowed / baseball$runs_scored

plot(baseball$runs_ratio,
     baseball$win_percentage,
     xlab = "Runs Allowed / Runs Scored",
     ylab = "Win Percentage",
     main = "Run Ratio vs Win Percentage")

model <- lm(win_percentage ~ runs_ratio, data = baseball)

abline(model)

summary(model)