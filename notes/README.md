git remote add [NAME I WANT] [URL to repo] #Another source that can connect to my local repository.
git fetch #this stores the remote information that is the latest but not integrating it into working
git merge [NAME I WANT]/master [or other branch] #this now puts the remote work into my work.

# always have to fetch and merge.
# 

cat("SYN_EMAIL=hathaway@datadriventeam.com\n",
    file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)
cat("SYN_PAT=https://www.synapse.org/#!Profile:3354800/settings\n",
    file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)
#Sys.getenv("SYN_EMAIL")
#Sys.getenv("SYN_PAT")

# latest R does things differently this is the fix
cat("R_MAX_VSIZE=64Gb\n",
    file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)

