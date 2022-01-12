#
# Data processor for the collision avoidance project
# ---------------------------------------------------

require(tidyr)
require(dplyr)
require(ggplot2)

# Replace wit something else for it to work locally
folder_name = './Projects/Behav-snippets-git/Sana project/'

file_name = paste(folder_name,'sana_collision_data.csv', sep='')

print(file_name)

# Read 2 lines of headers and the rest of data separately
exp_type = read.csv(file_name, skip = 0, header = F, nrows = 1, as.is = T)
i_tadpole = read.csv(file_name, skip = 1, header = F, nrows = 1, as.is = T)
data = read.csv(file_name, skip = 2, header = F, as.is = T)
print(exp_type)
print(i_tadpole)
print(data)

# BELOW: Pivot data,
# turn column numbers ('V1' etc) into a new column named 'id',
# then join information from the headers to it 
# (making them  columns, and using column number as id, 
# Then cleanup a bit (remove a duplicated column, 
# and turn tadpole id from string to a number).

d1 <- data %>% pivot_longer(-V1, names_to="id", values_to="response",
                            names_prefix = "V") %>%
  rename('trial' = V1) %>% 
  merge(pivot_longer(exp_type, everything(), names_prefix = "V", names_to="id", ), by='id') %>% 
  rename('group' = value) %>%
  merge(pivot_longer(i_tadpole, cols=-V1, names_prefix = "V", names_to="id", ), by='id') %>% 
  rename('i_tadpole' = value) %>%
  select(-V1) %>%
  mutate(id = as.numeric(id))

head(d1)

# Averages for each experiment
ds <- d1 %>% group_by(group, id) %>% summarize(m=mean(response))
dss <- ds %>% group_by(group) %>% summarize(m = mean(m))
ggplot(ds, aes(group, m, fill=group)) + 
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + 
  geom_jitter(aes(color=group), width=0.1, size=3) +
  geom_point(data=dss, shape=15, size=5) +
  theme_bw() +
  xlab('Experimental group') +
  ylab('Average repsonse frequency')

summary(aov(data=d1, response ~ trial+group))

# Was there habituation?
ds2 <- d1 %>% group_by(group,trial) %>% summarize(m = mean(response))
ggplot(ds2, aes(color=group, trial, m)) + 
  geom_point() +
  theme_bw() +
  geom_smooth(method='lm', se=F) # Not really

# Data consistency test (data printed back and manually compared to the original table)
for(i in 2:max(d1$id)){
  print(i)
  print(d1[d1$id==i,'group'][1])
  out = ''
  for(j in 1:16){
    out = cat(out,d1[(d1$id==i) & (d1$trial==j), 'response'])
  }
  print(out)
}
