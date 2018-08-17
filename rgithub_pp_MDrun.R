
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


## Readme's [MD]

readme_search = 'filename:"readme.md" path:"/"'


spec_search_term = "user:d3"
spec_search_term = "tensorflow tutorials"
spec_search_term = "tensorflow arXiv"
spec_search_term = "tensorflow arXiv cran"
spec_search_term = "tensorflow arXiv image"
spec_search_term = "tensorflow rstudio"
spec_search_term = "tensorflow R-CNN"
spec_search_term = "tensorflow segmentation mask"
spec_search_term = "tensorflow edge detection"

spec_search_term = "user:ddionrails user:mhebing user:paneldata"

spec_search_term = "user:cscheid"

spec_search_term = "ross girshick"

# > neue ideen
# coco dataset
# _ rg

( search_query = paste(spec_search_term, readme_search) )



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

# md
system.time( parser_res <- flat.search.parser(sr) )


# some parser evaluation
parser_res$type

parser_res$ok_vec
parser_res$path_vec
parser_res$score_vec

table(parser_res$ok_vec)



### III: Metadata extraction and analysis layer

Metainfos = parser_res$parsed_list

length(Metainfos)



### IV: to TManalyzer

( doc_names = parser_res$path_vec )


t_vec = meta.list.extract(Metainfos, doc_names)


# check
t_vec$t_vec
t_vec$doc_names



# add more stopwords by hand
more_stopwords = c("instal", "you", "your", "sudo", "will", "librari", "how", "run", "ansibl", "user", "our", "width", "each", "file",
					"have", "youll", "need", "like", "want", "not", "let", "pleas")

# system.time( A_list <- tm.create.models(t_vec, models = c("lsa")) )
# system.time( A_list <- tm.create.models(t_vec, stopwords_select = "cran", models = c("tt", "lsa")) )
system.time( A_list <- tm.create.models(t_vec, stopwords_select = "cran", add_stopwords = more_stopwords, models = c("tt", "lsa")) )
# take LSA standard (50% sv weight)
A = A_list$lsa



### V: TM applications : output to BitQueryS

## general for all data types

d = dist(A)

k = 5
# k = 6
# k = 16
# k = 24
# k = 40
# k = 48
# k = 50
# k = 64

# graphics.off()

# k-means
system.time( cl_r <- D3Visu_ClusterTopic_kmeans(A, k, 5, mds_plot = FALSE, topic_print = TRUE, d_mat = d) )


# pam
# system.time( cl_r <- D3Visu_ClusterTopic_pam(A, k, 5, mds_plot = F, topic_print = TRUE) )
system.time( cl_r <- D3Visu_ClusterTopic_pam(A, k, 5, mds_plot = F, topic_print = TRUE, d_mat = d) )


## MD output
search_query

# ( gh_path = sr$gh_path[parser_res$ok_vec] )
# ( gh_org = sr$gh_login[parser_res$ok_vec] )
# ( full_link = sapply( sr$full_search[parser_res$ok_vec], function(item){ item$repository$html_url } ) ) 
( full_link = sapply( sr$full_search, function(item){ item$repository$html_url } ) ) 

export_l = createD3_PreJSON_MD(Metainfos, sr$gh_path, sr$gh_login, full_link, cl_r$cl_topic_vec, cl_r$cluster)

d_out = list(nodes = export_l, links = list())

# message_str = "tensorflow_tutorials"
# message_str = "user_d3"
# message_str = "tensorflow_arXiv"
# message_str = "tensorflow_arXiv_image"
# message_str = "tensorflow_arXiv_image_1000_PAM"
# message_str = "tensorflow_rstudio_PAM_better_cnames"
# message_str = "tensorflow_R-CNN_PAM_better_cnames"
# message_str = "tensorflow_segmentation mask_PAM"
# message_str = "tensorflow_edge_detection_PAM"
# message_str = "test2_1911"
# message_str = "diw_t1"
# message_str = "user_cscheid"
# message_str = "ross_girshick"


( fname = paste("BQsearch/output/md_github_", tolower(message_str), "_", k, "cl.json", sep="") )


save_JSON(d_out, fname, showPretty = TRUE)


