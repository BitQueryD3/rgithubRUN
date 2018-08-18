
library(rgithubS)
## for TM analysis: from https://github.com/lborke/TManalyzer_dev
library(TManalyzer)


setwd("c:/r/github")

source("BQSearch/BitQuerySearchExport.R")

source("BQSearch/gh_login.R")
# "gh_login.R" führt nur diesen Code aus, Authentifizierungs-Parameter müssen dort zu Beginn manuell eingefügt werden
# ctx = interactive.login("XXX", "YYY", scopes=c("repo"))

# ctx$user$login
# ctx$user$id


## cran query examples

r_pack_search = 'Package Title Version Description Author filename:"DESCRIPTION" path:"/"'

spec_search_term = "text mining"
spec_search_term = "hornik"
spec_search_term = "tensorflow"
spec_search_term = "hadley wickham"
spec_search_term = "github api"
spec_search_term = "achim zeileis"

( search_query = paste(spec_search_term, r_pack_search) )



### I: Search API layer: 

## Full search

sr <- search.code.full(search_query, delay_l = 1)

sr <- search.code.full(search_query, delay_l = 1, print_stats = F)

sr <- search.code.full(search_query, delay_l = 1, max_items = 100)

sr <- search.code.full(search_query, delay_l = 1, max_items = 250)

# "preview" with 100 items, equivalent to simple "search.code"
sr <- search.code.full(search_query, delay_l = 1, max_items = 100, print_stats = F)

# default: gets maximally 1000 items with printing statistics
system.time( sr <- search.code.full(search_query, delay_l = 1) )


sort(table(sr$gh_login), decreasing = T)[1:10]
sort(table(sr$repo_name), decreasing = T)[1:10]
boxplot(sr$scores)



### II: Parser layer - V3 version

# cran/dcf
#system.time( parser_res <- dcf.search.parser(sr, error_to_results = FALSE) )

# for further filtering of 'sr$full_search'
system.time( parser_res <- dcf.search.parser(sr, error_to_results = TRUE) )


# some parser evaluation
parser_res$type
parser_res$error_count

parser_res$ok_vec
parser_res$path_vec
parser_res$score_vec

# error evaluation if "error_to_results = TRUE"
parser_res$path_vec[!parser_res$ok_vec]
parser_res$path_vec[parser_res$ok_vec]
# test: äq. zu Zeile zuvor
parser_res$path_vec[which(parser_res$ok_vec)]
# good list
parser_res$parsed_list[which(parser_res$ok_vec)]
# bad list
parser_res$parsed_list[which(!parser_res$ok_vec)]

table(parser_res$ok_vec)



### III: Metadata extraction and analysis layer

# Metainfos = parser_res$parsed_list

# take only items without parser errors, if "error_to_results = TRUE"
Metainfos = parser_res$parsed_list[which(parser_res$ok_vec)]

length(Metainfos)

# check bad meta infos
Metainfos[which(!parser_res$ok_vec)]


### some Meta analysis (optional)

metadata.analysis(Metainfos)

# cran
metadata.analysis(Metainfos, cran_dfield_list)


## some Meta processing/projection

meta_df_p = meta_dfield.projection(Metainfos, cran_dfield_list, c("p", "d", "t"))
meta_df_p = meta_dfield.projection(Metainfos, cran_dfield_list, c("p", "t", "v", "l", "ty"))
meta_df_p = meta_dfield.projection(Metainfos, cran_dfield_list, c("p", "v", "l", "m"))
meta_df_p = meta_dfield.projection(Metainfos, cran_dfield_list, c("p", "v", "m"))
meta_df_p = meta_dfield.projection(Metainfos, cran_dfield_list, c("p", "v", "a"))

( meta_df_p_frame = as.data.frame(meta_df_p) )


## Generalization
# add some parser/gh infos, works with any metadata format
meta_df_p$gh_path	= parser_res$path_vec[parser_res$ok_vec]
meta_df_p$score		= round(parser_res$score_vec[parser_res$ok_vec], 2)

( meta_df_p_frame = as.data.frame(meta_df_p) )

head(meta_df_p_frame)

# take k random rows from the data.frame
k = 5
meta_df_p_frame[sample(nrow(meta_df_p_frame), k),]



### IV: to TManalyzer

# smart cran
meta_weight = c(d = 9, t = 4, p = 1, a = 3, l = 1, m = 4)

# from parsed Metainfo
( doc_names	= sapply( Metainfos, function(meta){ meta.getDField(meta, cran_dfield_list, "p") } ) )

# tests
# t_vec = meta.list.extract(Metainfos[1:3], doc_names[1:3], cran_dfield_list, meta_weight)

# real extraction
t_vec = meta.list.extract(Metainfos, doc_names, cran_dfield_list, meta_weight)


# check
t_vec$t_vec
t_vec$doc_names



# add more stopwords by hand
more_stopwords = c("instal", "you", "your", "sudo", "will", "librari", "how", "run", "ansibl", "user", "our", "width", "each", "file",
					"have", "youll", "need", "like", "want", "not", "let", "pleas", "famili", "ctb", "email", "role", "aut", "persongiven")

# system.time( A_list <- tm.create.models(t_vec, models = c("lsa")) )
# system.time( A_list <- tm.create.models(t_vec, stopwords_select = "cran", models = c("tt", "lsa")) )
system.time( A_list <- tm.create.models(t_vec, stopwords_select = "cran", add_stopwords = more_stopwords, models = c("tt", "lsa")) )
# take LSA standard (50% sv weight)
A = A_list$lsa



### V: TM applications : output to BitQueryS

## general for all data types

d = dist(A)

k = 8
# k = 16
# k = 24
# k = 40
# k = 48
# k = 64

# graphics.off()

# k-means
system.time( cl_r <- D3Visu_ClusterTopic_kmeans(A, k, 5, mds_plot = FALSE, topic_print = TRUE, d_mat = d) )


# pam
# system.time( cl_r <- D3Visu_ClusterTopic_pam(A, k, 5, mds_plot = F, topic_print = TRUE) )
system.time( cl_r <- D3Visu_ClusterTopic_pam(A, k, 5, mds_plot = F, topic_print = TRUE, d_mat = d) )


## CRAN output

( gh_org = sr$gh_login[which(parser_res$ok_vec)] )
( full_link = sapply( sr$full_search[which(parser_res$ok_vec)], function(item){ item$repository$html_url } ) )

c(length(gh_org), length(full_link)) == length(Metainfos)


# Export to JSON using the topic labels generated by the TM process + D3Visu_ClusterTopic
export_l = createD3_PreJSON_CRAN(Metainfos, gh_org, full_link, cl_r$cl_topic_vec, cl_r$cluster)

d_out = list(nodes = export_l, links = list(), search_query = search_query)

# message_str = "LSA"
# message_str = "LSA_t2"
# message_str = "test1"
# message_str = "test2"
# message_str = "gh_api"
# message_str = "zeileis"


( fname = paste("BQsearch/output/cran_github_", tolower(message_str), "_", k, "cl.json", sep="") )


save_JSON(d_out, fname, showPretty = TRUE)

