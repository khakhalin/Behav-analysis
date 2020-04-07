#
# OMR analysis for Rachel's dataset
#
# ------------------------------------

require(ggplot2)
require(tidyr)
require(dplyr)

# Change this folder to run it locally
folder <- './Projects/Behav-snippets-git/Rachel project/'

d <- read.csv(paste(folder, 'omr_data.csv', sep=''))
names(d)

# Some Left/Right labels have spaces in them, clean up:
d <- d %>% mutate(Direction = factor(trimws(Direction)))
summary(d)

# How to measure OMR? Say p is the probability of changing the side spontaneously,
# and x is the probability of change induced by the stimulus. Then:
# rnew = r(1-p) + (1-r)(p+x-px)
# Where rnew is the number on the right after the stimulus,
# and r is the number before (assuming l>r stimulus).
# From it:
# x = ((rnew-r(1-p))/(1-r) - p)/(1-p)
# If p==0, then omr = delta(r)/(1-r) - just simply the % that moved

d <- d %>% mutate(Before = ifelse(Direction=='Left',Left_before,5-Left_before),
                  After  = ifelse(Direction=='Left',Left_after,5-Left_after))
summary(d)

ds <- d %>% group_by(Group) %>% summarize(
  Treatment=first(Treatment),
  Before = mean(Before),
  After = mean(After))
head(ds)

ds <- ds %>% mutate(Change = (After-Before)/(5-Before))
summary(ds)

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