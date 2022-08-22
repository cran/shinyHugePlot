
#' @description
#' Generate a plotly object including Javascript,
#' which passes the relayout information to \code{Shiny}
plotly_with_relayout = function(){
  js <- "
        function(el, x){
          var id = el.getAttribute('id');
          document.getElementById(id).on('plotly_relayout', function(ev){
            console.log(ev);
            ev['outputId'] = id;
            Shiny.setInputValue(id + '_relayout', ev);
            })
        }
      "

  self$figure %>%
    htmlwidgets::onRender(js)
}
