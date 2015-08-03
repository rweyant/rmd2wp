
knit2wordpress <- function(file,syntax.highlighter=NULL) {
  require(XML)
  post.content <- readLines(file)

  ## Operations common to all highlighters
  post.content <- gsub(" <", "&nbsp;<", post.content)
  post.content <- gsub("> ", ">&nbsp;", post.content)
  post.content <- gsub("<br/>", "&nbsp;", post.content)

  post.content <- htmlTreeParse(post.content)
  post.content <- paste(capture.output(print(post.content$children$html$children$body,
                                             indent = FALSE, tagSeparator = "")), collapse = "\n")
  ## Change to actual apostrophes
  post.content <- gsub('&apos;',"'",post.content)

  ## Somewhere this tag is getting inserted and is uninterpretable
  post.content <- gsub("<br/>", "&nbsp;", post.content)


#   post.content <- gsub("<pre><code class=\"r\">","\\[code lang=\'r\'\\]",post.content)
#   post.content <- gsub("<pre><code class=\"python\">","\\[code lang=\'python\'\\]",post.content)
#
#   post.content <- gsub("</code></pre>","\\[/code\\]",post.content)


  if (!is.null(syntax.highlighter)){
    post.content <- gsub("<pre><code>","\\[code]",post.content)
    post.content <- gsub("<pre><code class=\"r\">","\\[code lang=\'r\'\\]",post.content)
    post.content <- gsub("<pre><code class=\"python\">","\\[code lang=\'python\'\\]",post.content)

    post.content <- gsub("</code></pre>","\\[/code\\]",post.content)


    ## Sometimes you get [code] tags put around each line in a code block.
    ## This makes them all part of one code block
    post.content <- gsub("\\[/code\\]\n\\[code\\]", "\n", post.content)
    post.content <- gsub("\\[/code\\]\n\\[code lang=\'r\'\\]", "\n", post.content)
    post.content <- gsub("\\[/code\\]\n\\[code lang=\'python\'\\]", "\n", post.content)
}
  if (syntax.highlighter %in% c('WP-Syntax','WP-CodeBox')){
    # Translate [code] blocks to <pre> blocks
    post.content <- gsub("\\[code\\]", "<pre lang=\"R\" line=\"1\">", post.content)
    post.content <- gsub("\\[code lang=\'r\'\\]", "<pre lang=\"rsplus\" line=\"1\">", post.content)
    post.content <- gsub("\\[/code]", "</pre>", post.content)

  } else if (!(syntax.highlighter %in% c('WP-Syntax','WP-CodeBox','Syntax Highlighter Evolved'))){
    print("Need valid Syntax Highlighter.")
  }
  return(post.content)
}
