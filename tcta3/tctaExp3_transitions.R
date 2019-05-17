# tctaExp3_transitions.R
# Jessica Tin
# 20 Aug 2018
#
# Creates balanced order of conditions for TCTA-Exp3.
# Outputs CSV with 432+2 trials.
#
# Balanced: Talker+words (27 of each talker+word combination)
#           Talker+spans (4 sets of each talker+span combination)
#           (n.b. balancing the above also balances talkers, words, and spans:
#               Talkers (108 trials/talker)
#               Words (108 trials/word)
#               Spans (16 of each span size))
#           Word-to-word transitions (27 of each word-to-word transition)
#           Talker-to-talker transitions (8 of each talker-to-talker transition)
#           Span-to-talker transition (4 of each span-to-talker transition)
#
# How it works: See "CALCULATE NUMBER OF TRIALS[...]" section for calculations.
#   This script generates ordered, balanced trials by first populating lists with
#   the proper number of each thing to be balanced (talkers+word combinations,
#   span-to-talker transitions, etc.), such that if every element in those lists
#   is used, the resulting list will be balanced. (e.g. for 16 talker+word
#   combinations to be balanced in an experiment of 432 trials, each talker+word
#   combination must occur 27 times; so the talker_words_remaining list is
#   prepopulated with 27 of each combination.)
#   The script then loops through the lists, selecting one element from the
#   relevant list(s) until every element has been used. If there is no valid
#   choice remaining in a list, the loop breaks and restarts.
#   The resulting lists include the exact number* of each element needed for the
#   experiment to be balanced.
#   *2 extra trials are added to balance the transitions (since at least 1 "extra"
#   trial is needed for the final transitions, and the minimum span size is 2
#   trials.) Therefore, one word occurs in 1 more trial than the other words, and
#   one talker occurs in 2 more trials than the other talkers.
#

#### LOAD PACKAGES ####
if(!require("dplyr")) {install.packages("dplyr"); library(dplyr)}
if(!require("readr")) {install.packages("readr"); library(dplyr)}

#### SET WORKING DIRECTORY ####
setwd("/Volumes/PerrachioneLab/projects/TCTA/Experiment/experiment3") # Mac
#setwd("/PerrachioneLab/projects/TCTA/Experiment/experiment3") # Linux
#setwd("R:/PerrachioneLab/projects/TCTA/Experiment/experiment3") # Windows

#### DEFINE STIMULUS PARAMETERS ####
talkers <- c("A", "B", "C", "D")
words <- 1:4
word_strings <- c("boot", "boat", "bet", "bat")
span_lengths <- 2:7
consecutive_word_limit <- 3 # no more than 3 of the same word in a row

#### ENUMERATE COMBINATIONS AND TRANSITIONS TO BE BALANCED ####
# talker+word combinations (i.e. A1, A2, A3, ..., D3, D4)
talker_words <- paste0(rep(talkers, each = length(words)),
                       rep(words, times = length(talkers)))

# talker+span combinations (i.e. A2, A3, A4, ..., D_6, D7)
talker_spans <- paste0(rep(talkers, each = length(span_lengths)),
                       rep(span_lengths, times = length(talkers)))

# word-to-word transitions (i.e. 1_1, 1_2, 1_3, ..., 4_3, 4_4)
ww_transitions <- paste(rep(words, each = length(words)),
                        rep(words, times = length(words)), sep = "_")

# talker-to-talker transitions (i.e. A_B, A_C, A_D ..., D_B, D_C)
# (can't use paste(rep, rep) method or same-talker transitions will be included)
tt_transitions <- unlist(combn(talkers, 2, simplify = FALSE))
tt_transitions <- c(paste(tt_transitions[seq.int(1,length(tt_transitions),2)],
                          tt_transitions[seq.int(2,length(tt_transitions),2)],
                          sep = "_"),
                    paste(tt_transitions[seq.int(2,length(tt_transitions),2)],
                          tt_transitions[seq.int(1,length(tt_transitions),2)],
                          sep = "_"))

# span-to-talker transitions (i.e. 2_A, 2_B, 2_C, ..., 7_C, 7_D)
st_transitions <-  paste(rep(span_lengths, each = length(talkers)),
                         rep(talkers, times = length(span_lengths)), sep = "_")

#### CALCULATE NUMBER OF TRIALS NEEDED FOR BALANCE ####
length(span_lengths)   # 6 unique spans
sum(span_lengths)      # 27 trials per span set

length(words)          # 4 unique words
                       # 4 trials per word set

length(talkers)        # 4 unique talkers
                       # 4 trials per talker set

length(talker_words)   # 16 unique talker+word combinations

length(ww_transitions) # 16 unique word-to-word transitions
length(tt_transitions) # 12 unique talker-to-talker transitions

length(talker_spans)   # 24 unique talker+span combinations
length(st_transitions) # 24 unique span-to-talker transitions

# TOTAL NUMBER OF TRIALS
# lcm(6, 27, 4, 16, 24) = 432 trials needed to balance everything above

# SPANS
# 432 trials / 27 trials per span set = 16 of each span set
# 16 span sets * 6 spans per span set = 96 spans

# TALKER_WORDS
# n.b. if talker_words are balanced, then talkers and words are each balanced
# 432 trials / 16 talker+word combinations = 27 of each talker+word

# TALKER_SPANS
# n.b. if talker_spans are balanced, then talkers and spans are each balanced
# 16 of each span set / 4 talkers = 4 of each talker+span

# WW_TRANSITIONS
# n.b. if ww_transitions are balanced, then words are balanced
# 432 trials / 16 word-to-word transitions = 27 of each word-to-word transition

# TT_TRANSITIONS
# n.b. if tt_transitions are balanced, then talkers are balanced
# 96 blocks / 12 talker-to-talker transitions = 8 of each talker-to-talker transition

# ST_TRANSITIONS
# n.b. if st_transitions are balanced, then spans and talkers are each balanced
# 96 blocks / 24 span-to-talker transitions = 4 of each span-to-talker transition

#### SET TALKER AND SPAN ORDER ####
talker_span_list <- c()
while(length(talker_span_list) < 96) { # 96 spans as calculated above
# outer while loop: starting over completely (span 1)
    print("Balancing talkers and spans (starting new attempt)")

    # start by resetting all the lists
    talker_span_list <- c()
    talker_spans_remaining <- rep(talker_spans, 4) # 4 of each talker+span
    tt_transitions_remaining <- rep(tt_transitions, 8) # 8 of each talker-to-talker
    st_transitions_remaining <- rep(st_transitions, 4) # 4 of each span-to-talker

    # SPAN 1
    # for the first span, can choose talker and span length at random
    # without worrying about balanced transitions, etc.)

    # choose TALKER and SPAN LENGTH
    talker_span <- sample(talker_spans_remaining, 1)
    talker <- substr(talker_span, 1, 1)
    span_length <- substr(talker_span, 2, 2)

    # update lists
    talker_span_list <- c(talker_span_list, list(list(talker, span_length)))
    talker_spans_remaining <-
        talker_spans_remaining[-match(talker_span, talker_spans_remaining)]

    # SPANS 2-96
    while(length(talker_span_list) < 96) { # inner while loop: spans 2-96
        # check transitions first
        possible_talker_spans <- talker_spans_remaining[
            !startsWith(talker_spans_remaining, talker) &
                (substr(talker_spans_remaining, 1, 1) %in%
                     intersect(
                         substr(tt_transitions_remaining[
                             startsWith(tt_transitions_remaining, talker)], 3, 3),
                         substr(st_transitions_remaining[
                             startsWith(st_transitions_remaining, span_length)], 3, 3)
                     )
                )
            ]

        if(length(possible_talker_spans) == 0) {
            #print("Breakpoint 1 - talkers and spans")
            break
        }

        # else, choose TALKER and SPAN LENGTH
        talker_span <- sample(possible_talker_spans, 1)

        # update values
        tt_transition <- paste(talker, substr(talker_span, 1, 1), sep = "_")
        st_transition <- paste(span_length, substr(talker_span, 1, 1), sep = "_")
        talker <- substr(talker_span, 1, 1)
        span_length <- substr(talker_span, 2, 2)

        # update lists
        talker_span_list <- c(talker_span_list, list(list(talker, span_length)))
        talker_spans_remaining <-
            talker_spans_remaining[-match(talker_span, talker_spans_remaining)]
        tt_transitions_remaining <-
            tt_transitions_remaining[-match(tt_transition, tt_transitions_remaining)]
        st_transitions_remaining <-
            st_transitions_remaining[-match(st_transition, st_transitions_remaining)]
    } # inner while loop: spans 2-96

    print(paste("Ended with span", length(talker_span_list)))
} # outer while loop: starting over completely (span 1)

# if the above loops finished, then the full set of 96 talkers and spans was
# (hopefully) properly balanced!
#print(talker_span_list)

# n.b. Since a transition requires two trials, there is one "extra" talker-to-talker
# transition and span-to-talker transition left over at the end of the balancing.
# The talker designated by the talker-to-talker transition balance should be the
# same as the talker designated by the span-to-talker transition balance.
cat(paste("Final talker:", talker,
          "\nFinal span length:", span_length,
          "\nRemaining talker-to-talker transition:", tt_transitions_remaining,
          "\nRemaining span-to-talker transition:", st_transitions_remaining))

# add the remaining talker+span (set "extra" span to 2 trials)
talker <- substr(tt_transitions_remaining, 3, 3)
span_length <- 2
talker_span_list <- c(talker_span_list, list(list(talker, span_length)))
#print(talker_span_list)

#### SET WORD ORDER ####
talker_words_remaining <- rep(talker_words, 27) # 27 of each talker+word
ww_transitions_remaining <- rep(ww_transitions, 27) # 27 of each word-to-word
word_list <- c()
talker_list <- strrep(unlist(talker_span_list)[seq.int(1, 193, 2)],
                      as.integer(unlist(talker_span_list)[seq.int(2, 194, 2)])) %>%
    strsplit(split="") %>%
    unlist()

while(length(word_list) < 432) {
# outer while loop: starting over completely (trials 1-2)
    print("Balancing words (starting new attempt)")

    # start by resetting all the lists
    word_list <- c()
    talker_words_remaining <- rep(talker_words, 27) # 27 of each talker+word
    ww_transitions_remaining <- rep(ww_transitions, 27) # 27 of each word-to-word

    # WORDS 1-2
    # for the first two words, we don't have to check anything (can choose word-to-word
    # transition at random without worrying about balance)

    # choose WORD TRANSITION (thereby choosing WORDS)
    word_transition <- sample(ww_transitions_remaining, 1)
    ww_transitions_remaining <-
        ww_transitions_remaining[-match(word_transition, ww_transitions_remaining)]
    word <- substr(word_transition, 1, 1)
    next_word <- substr(word_transition, 3, 3)

    # keep track of consecutive words
    if(word == next_word) {
        consecutive_words <- 2
    } else {
        consecutive_words <- 1
    }

    # update lists
    talker <- talker_list[1]
    talker_words_remaining <-
        talker_words_remaining[-match(paste0(talker, word), talker_words_remaining)]
    talker_words_remaining <-
        talker_words_remaining[-match(paste0(talker, next_word), talker_words_remaining)]
    word_list <- c(word, next_word)

    # WORDS 3-432
    for(i in 3:432) { # inner while loop: trials 3-432
        talker <- talker_list[i]
        # check transitions first
        possible_ww_transitions <-
            ww_transitions_remaining[
                startsWith(ww_transitions_remaining, next_word) &
                    (substr(ww_transitions_remaining, 3, 3) %in%
                         unique(substr(grep(talker, talker_words_remaining,
                                            value = TRUE), 2, 2)))
                ]

        if(length(possible_ww_transitions) == 0) {
            # if no valid word-to-word transitions remain for the current talker,
            # then: break
            #print("Breakpoint 2 - words and talkers")
            break
        } else if(consecutive_words < consecutive_word_limit) {
            # else, if the same word can be chosen,
            # then: choose any word-to-word transition
            ww_transition <- sample(possible_ww_transitions, 1)
        } else if(length(
            possible_ww_transitions[!endsWith(possible_ww_transitions,
                                              next_word)]) == 0) {
            # else, if the same word can't be chosen, and there are no different-word
            # transitions remaining,
            # then: break
            #print("Breakpoint 3 - consecutive words")
            break
        } else {
            # else, if the same word can't be chosen, and there IS a different-word
            # transition remaining,
            # then: choose any different-word word-to-word transition
            ww_transition <- sample(
                possible_ww_transitions[!endsWith(possible_ww_transitions, next_word)], 1)
        }

        # keep track of consecutive words
        if(endsWith(ww_transition, next_word)) {
            consecutive_words <- consecutive_words + 1
        } else {
            consecutive_words <- 1
        }

        # update values
        next_word <- substr(ww_transition, 3, 3)

        # update lists
        ww_transitions_remaining <-
            ww_transitions_remaining[-match(ww_transition, ww_transitions_remaining)]
        talker_words_remaining <-
            talker_words_remaining[-match(paste0(talker, next_word), talker_words_remaining)]
        word_list <- c(word_list, next_word)
    } # inner while loop: trials 3-432

    print(paste("Ended with word", i))
} # outer while loop: starting over completely (trials 1-2)

# if the above loops finished, then the full list of 432 words was created, and
# is (hopefully) properly balanced!
#print(word_list)

# n.b. Since a transition requires two trials, there is one "extra" word-to-word
# transition left over at the end of the balancing.
cat(paste("Final word:", next_word,
          "\nRemaining word-to-word transition:", ww_transitions_remaining))

# add the remaining word, plus another random (different) word for the final trials
word_list <- c(word_list, substr(ww_transitions_remaining, 3, 3),
               sample(words[words != substr(ww_transitions_remaining, 3, 3)], 1))

#### CREATE DATA FRAME WITH TRIAL INFO ####
trials <- data.frame(span = as.integer(unlist(talker_span_list)[seq.int(2, 194, 2)]),
                     prev_span = c(NA, as.integer(unlist(talker_span_list)[seq.int(2, 193, 2)])))

# add sum_prev_span column (for rep_within_span calculation later)
trials$sum_prev_span <- as.integer(c(0, cumsum(na.omit(trials$prev_span))))

# replicate rows for each span length
trials <- trials[rep(seq_len(97), trials$span),]
rownames(trials) <- NULL

trials %<>% mutate(talker = talker_list,
                   word = word_strings[as.integer(word_list)],
                   trial = 1:nrow(trials),
                   rep_within_span = trial - sum_prev_span) %>%
    select(trial, talker, word, span, rep_within_span, prev_span) # reorder columns

#### CHECK BALANCES ####
# CHECK TALKER BALANCE
print("Number of trials per talker:")
print(count(trials, talker))

# CHECK SPAN BALANCE
print("Number of each span size:")
count(trials, span) %>% mutate(n = n/span)

# CHECK TALKER+SPAN BALANCE
print("Number of each talker+span combination:")
count(trials, talker, span) %>% mutate(n = n/span)

# CHECK WORD BALANCE
print("Number of trials per word:")
count(trials, word)

# CHECK TALKER+WORD BALANCE
print("Number of trials per talker+word combination:")
count(trials, talker, word)

# CHECK CONSECUTIVE TALKERS
rle(rle(paste(trials$talker))$values)
print(paste("Any consecutive spans of same talker:",
            any(rle(rle(paste(trials$talker))$values)$lengths > 1)))

# CHECK CONSECUTIVE WORDS
rle(paste(trials$word))
print(paste("Max consecutive trials of same word:",
            max(rle(paste(trials$word))$lengths)))

# CHECK SPAN-TO-TALKER TRANSITIONS
st <- paste(unlist(talker_span_list)[seq.int(2, 194, 2)],
            unlist(talker_span_list)[seq.int(3, 194, 2)], sep = "_")
table(st)

# CHECK TALKER-TO-TALKER TRANSITIONS
tt <- c()
for(i in 1:97) {
    tt <- c(tt, paste(rle(rle(paste(trials$talker))$values)$values[i],
                      rle(rle(paste(trials$talker))$values)$values[i + 1],
                      sep = "_"))
}
table(tt)

# CHECK WORD-TO-WORD TRANSITIONS
ww <- c()
for(i in 1:434) {
    ww <- c(ww, paste(trials$word[i], trials$word[i+1], sep = "_"))
}
table(ww)

#### SAVE CSV ####
write_csv(trials, "tcta_Exp3.csv")
