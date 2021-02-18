#' Use Parse API
#'
#' @seealso \url{https://developer.yahoo.co.jp/webapi/jlp/ma/v1/parse.html}
#'
#' @param sentence character vector.
#' @param results character scalar. choose one of `ma` or `uniq`.
#' @param filter numeric vector.
#' @param uniq_by_baseform logical.
#' @param app_id character scalar.
#' @return return results as a list.
#'
#' @import httr
#' @import dplyr
#' @import purrr
#' @importFrom stringi stri_enc_toutf8
#' @importFrom xml2 xml_children
#' @importFrom xml2 as_list
#' @export
jlp_parse <- function(sentence,
                      results = "ma",
                      filter = c(1:13),
                      uniq_by_baseform = TRUE,
                      app_id = Sys.getenv("YJDN_CLIENT_ID")) {

  response <- httr::POST(
     "https://jlp.yahooapis.jp/MAService/V1/parse",
     config = httr::user_agent(paste0("Yahoo AppID: ", app_id)),
     body = list(
       sentence = paste0(stringi::stri_enc_toutf8(sentence), collapse = " "),
       results = ifelse(results == "uniq", "uniq", "ma"),
       response = "feature",
       filter = paste0(as.character(filter), collapse = "|"),
       uniq_by_baseform = as.character(uniq_by_baseform)
     ),
     encode = "form"
  )

  if (response$status_code != 200) {
    message(response)
    return(invisible(list()))
  } else {
    res <- response %>%
      httr::content(as = "parsed", type = "text/xml", encoding = "UTF-8") %>%
      xml2::xml_children() %>%
      xml2::as_list() %>%
      purrr::flatten()

    res$total_count <- as.integer(unlist(res$total_count))
    res$filtered_count <- as.integer(unlist(res$filtered_count))

    res$word_list <- res$word_list %>%
      purrr::map_dfr(function(word) {
        df <- data.frame(feature = unlist(word$feature))
        if (results == "uniq") {
          df <- data.frame(df, count = as.integer(unlist(word$count)))
        }
        df <- tidyr::separate(
          df,
          "feature",
          into = c(
            "pos1",
            "pos2",
            "pos3",
            "surface",
            "reading",
            "baseform"
          ),
          sep = ","
        )
        return(df)
      })
    res$word_list <- res$word_list %>%
      dplyr::transmute(dplyr::across(where(is.character), ~ dplyr::case_when(
        . == "*" ~ NA_character_,
        . == "" ~ NA_character_,
        TRUE ~ .
      )))

    return(res)
  }
}


