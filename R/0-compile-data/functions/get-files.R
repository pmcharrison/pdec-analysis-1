get_files <- function(subj) {
  list(trials = tibble(subj = subj,
                       name = sprintf("stimDescription_subj_%i_exp1_retention.txt",
                                      subj),
                       path = file.path("input/exp1_retention/stimuli/",
                                        name)),
       stimuli = tibble(subj = subj,
                        name = sprintf("stimuli_subj_%i_exp1_retention.txt",
                                       subj),
                        path = file.path("input/exp1_retention/stimuli/",
                                         name)))
  
}
