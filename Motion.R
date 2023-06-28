#reading the fd data in
#To run this, use set_wd 
#Files are in OHSU_motion_Jerod

file_to_visit <- function(x) basename(x) %>% str_replace_all("\\.txt$","")
read_fd <- function(x) fread(x) %>% mutate(visit=file_to_visit(x), tp=1:n())

file_list <- list.files(pattern="\\.txt$")
all_fd_long <- lapply(file_list, read_fd) %>% bind_rows

# and then spread or pivot_wider to get to wide format

fd_wide <-all_fd_long %>%
  pivot_wider(names_from = tp, values_from = V1)

fd_wide <- subset(fd_wide, select = -c(`1`) )

# Take a mean against rows

fd_mean <- fd_wide %>% mutate(mean = rowMeans(across(where(is.numeric)), na.rm = TRUE))

# Reformat visit so it can be added to the other arrays

fd_mean$visit=fd_mean$visit=str_extract(fd_mean$visit,'\\d+')
#y <- x %>% mutate(mean = rowMeans(across(where(is.numeric)), na.rm = TRUE))

motion = data.frame(fd_mean$visit,fd_mean$mean)
names(motion) <- c("idchild", "mean_fd")
