

createD3_PreJSON_CRAN = function(meta_list, gh_org, full_link, cl_topic_vec = NULL, cluster = NULL, debug = FALSE) {

	export_l = list()

	for (i in 1:length(meta_list)) {
		meta = meta_list[[i]]
		
		if (!is.null(cl_topic_vec) && !is.null(cluster)) {
			artist_str = cl_topic_vec[cluster[i]]
		} else {
			# artist_str = expandBook(yaml.getQField(meta, "p"))
			artist_str = gh_org[i]
		}
	
		new_el = list(
					  name = meta.getDField(meta, cran_dfield_list, "p"),
					  artist = artist_str,
					  #canonical enumeration
					  id = i,
					  #size of the metainfo file (Byte)
					  playcount = sum(nchar(meta)),
					  description = paste(meta.getDField(meta, cran_dfield_list, "p"),
										  meta.getDField(meta, cran_dfield_list, "d"),
										  meta.getDField(meta, cran_dfield_list, "t"),
										  meta.getDField(meta, cran_dfield_list, "a")),
					  # multi SW mode
					  software = "r",
					  book = gh_org[i],
					  full_link = full_link[i]
				 )
		if (debug) { print(new_el) }
		export_l = c(export_l, list(new_el))
	}
	return(export_l)
}


createD3_PreJSON_Gitenberg = function(meta_list, gh_org, full_link, cl_topic_vec = NULL, cluster = NULL, debug = FALSE) {

	export_l = list()

	for (i in 1:length(meta_list)) {
		meta = meta_list[[i]]
		
		if (!is.null(cl_topic_vec) && !is.null(cluster)) {
			artist_str = cl_topic_vec[cluster[i]]
		} else {
			artist_str = gh_org[i]
		}
	
		new_el = list(
					  name = meta.getDField(meta, gitenberg_dfield_list, "t"),
					  artist = artist_str,
					  #canonical enumeration
					  id = i,
					  #size of the metainfo file (Byte)
					  playcount = sum(nchar(meta)),
					  description = paste(meta.getDField(meta, gitenberg_dfield_list, "t"),
										  meta.getDField(meta, gitenberg_dfield_list, "d"),
										  meta.getDField(meta, gitenberg_dfield_list, "s"),
										  meta.getDField(meta, gitenberg_dfield_list, "c")),
					  # multi SW mode
					  software = "gitenberg",
					  book = gh_org[i],
					  full_link = full_link[i]
				 )
		if (debug) { print(new_el) }
		export_l = c(export_l, list(new_el))
	}
	return(export_l)
}


createD3_PreJSON_NPM_PHP = function(meta_list, meta_dfield_list, gh_org, full_link, cl_topic_vec = NULL, cluster = NULL, debug = FALSE) {

	export_l = list()

	for (i in 1:length(meta_list)) {
		meta = meta_list[[i]]
		
		if (!is.null(cl_topic_vec) && !is.null(cluster)) {
			artist_str = cl_topic_vec[cluster[i]]
		} else {
			artist_str = gh_org[i]
		}
		
		if (class(meta_dfield_list) == "php") {software_str = "php"} else
		if (class(meta_dfield_list) == "npm") {software_str = "npm"} else {software_str = "npm"}
	
		new_el = list(
					  name = meta.getDField(meta, meta_dfield_list, "n"),
					  artist = artist_str,
					  #canonical enumeration
					  id = i,
					  #size of the metainfo file (Byte)
					  playcount = sum(nchar(meta)),
					  description = paste(meta.getDField(meta, meta_dfield_list, "n"),
										  meta.getDField(meta, meta_dfield_list, "d"),
										  meta.getDField(meta, meta_dfield_list, "k"),
										  meta.getDField(meta, meta_dfield_list, "a")),
					  # multi SW mode
					  software = software_str,
					  book = gh_org[i],
					  full_link = full_link[i]
				 )
		if (debug) { print(new_el) }
		export_l = c(export_l, list(new_el))
	}
	return(export_l)
}


createD3_PreJSON_MD = function(meta_list, gh_path, gh_org, full_link, cl_topic_vec = NULL, cluster = NULL, debug = FALSE) {

	export_l = list()

	for (i in 1:length(meta_list)) {
		meta = meta_list[[i]]
		
		if (!is.null(cl_topic_vec) && !is.null(cluster)) {
			artist_str = cl_topic_vec[cluster[i]]
		} else {
			# artist_str = expandBook(yaml.getQField(meta, "p"))
			artist_str = gh_org[i]
		}
		
		meta_cleaned = gsub("[^A-Za-z0-9]", " ", meta)
		playcount_cleaned = sum(nchar(meta))
		if (is.na(playcount_cleaned) | playcount_cleaned <= 0 ) {playcount_cleaned = 1}
	
		new_el = list(
					  name = gh_path[i],
					  artist = artist_str,
					  #canonical enumeration
					  id = i,
					  #size of the metainfo file (Byte)
					  playcount = playcount_cleaned,
					  # description = meta,
					  # description = str_replace_all(meta, "[^[:alnum:]]", " "),
					  description = paste(gh_path[i], artist_str, meta_cleaned),
					  # multi SW mode
					  software = "md",
					  book = gh_org[i],
					  full_link = full_link[i]
				 )
		if (debug) { print(new_el) }
		export_l = c(export_l, list(new_el))
	}
	return(export_l)
}

