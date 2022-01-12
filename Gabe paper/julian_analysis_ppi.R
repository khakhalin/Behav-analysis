#
# The analysis for PPI data from Julian's and Addie's work
#
# ------------------------------------

require(ggplot2)
require(tidyr)
require(dplyr)

# Change this folder to run it locally
folder <- './Projects/Behav-snippets-git/Gabe pape/'

d <- read.csv(paste(folder, 'ppi.csv', sep=''))
names(d)

# Lets look at habituation
# The first 4 columns are real, the rest need to be melted
d <- d %>% pivot_longer(cols=starts_with("X"), names_prefix="X", names_to="Stimulus", values_to="Response")
d$date = factor(d$date)
d$Stimulus = as.numeric(d$Stimulus)
summary(d)

# Visualize
ggplot(data=d, aes(Stimulus, Response, color=group)) + theme_classic() +
  geom_point() + geom_smooth(method='lm', se=F)

summary(aov(data=d, Response~Stimulus*group)) # p=0.07



ggplot(data=ds,aes(Treatment,Change, fill=Treatment)) + 
  theme_classic() + geom_hline(yintercept=0, color='gray') +
  geom_dotplot(binaxis='y', stackdir='center', bins=50) +
  ylab('Mean movement')

model = aov(data=ds, Change~Treatment)
summary(model)
TukeyHSD(model) # Not surprising, witn n=4...

# But is there a total effect?
t.test(ds$Change)


# -------- A bit incorrect analysis (each run becomes a point)
d <- d %>% mutate(Change = (After-Before)/(5-Before))
summary(d)

ggplot(data=d,aes(Treatment,Change, color=Treatment)) + 
  theme_classic() + geom_hline(yintercept=0, color='gray') +
  geom_jitter(height=0.1, width=0.1) +
  ylab('Mean movement')

model = aov(data=d, Change~Treatment)
summary(model)
TukeyHSD(model) # Same thing. Good to know.

# -------- Bad nested analysis (account for within-group variation)
ggplot(data=d,aes(Treatment,Change, color=factor(Group))) + 
  theme_classic() + geom_hline(yintercept=0, color='gray') +
  geom_jitter(height=0.1, width=0.1) +
  ylab('Mean movement')
model = aov(data=d, Change~Treatment+factor(Group))
summary(model)
TukeyHSD(model, which='Treatment') # As expected, no effect