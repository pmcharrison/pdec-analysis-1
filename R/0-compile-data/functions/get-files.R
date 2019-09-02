get_files <- function(subj, 
                      path_stim,
                      format_stim,
                      format_stim_desc) {
  
  list(trials = tibble(subj = subj,
                       name = sprintf(format_stim_desc, subj),
                       path = file.path(path_stim, name)),
       
       stimuli = tibble(subj = subj,
                        name = sprintf(format_stim, subj),
                        path = file.path(path_stim, name)))
  
}
