function(input, output, session) {
  
  login <- reactiveValues(login = FALSE, user = NULL, role = NULL, email = NULL)
  
  # initially display the login modal
  observe({
    composeLoginModal()
  })
  
  observeEvent(input$logout_ok, {
    shiny::removeModal()
    
    # clear the values when logout is confirmed
    login$login <- FALSE
    login$user  <- NULL
    login$role  <- NULL
    login$email <- NULL
    
    composeLoginModal(
      div(
          id    = "modal-logout-message"
        , style = "margin-bottom: 10px"
        , span(class = "text-muted", "Successfully Logged Out")
      ) #/ modal-logout-message
    ) #/ composeLoginModal
  })
  
  # once a login is attempted, do some checks
  observeEvent(input$login_button, {
    
    # remove the modal while we check
    shiny::removeModal()
    
    # query the database for that user will return NAs if not populated
    stored <- sendUserGetQuery(input$login_user)
    
    # if any are NA then the record doesn't exist or the record is corrupted
    user_invalid <- stored %>% sapply(is.na) %>% any
    
    # try to login, will automatically handle NULL-y objects
    login$login <- validateLogin(stored$password, input$login_passwd)
    
    # if the login is not successful, toss up another login modal, 
    # this time with a message
    if (isTRUE(user_invalid) | login$login == FALSE) {
      composeLoginModal(
        div(
            id    = "modal-login-message"
          , style = "margin-bottom: 10px"
          , span(style = "color: red; font-weight:bold", "Incorrect Login/Password")
          ) #/ modal-login-message
        ) #/ composeLoginModal
    } else {
      # if the login is successful, populate the known values
      login$user  <- stored$user
      login$role  <- stored$role
      login$email <- stored$email
      
      rm(stored)
    } #/ fi
  }) #/ login_button Observer
  
  # close database conncention on exit
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
  
  observeEvent(input$logout, {
    helpText("Are you sure you want to Logout? Any unsaved work will be lost!") %>%
      div(style = "margin-bottom: 15px", .) %>%
      showConfirmModal("logout", .)
  })
  
  observeEvent(input$logout_cancel, {
    shiny::removeModal()
  })
  
  output$welcome <- renderUI({
    # wait to render until the login is truthy
    req(login$login)
    
    # a crude login card
    div(style = "width: 160px; margin-top: 15px"
      , wellPanel(
        img(src = "avatar.png")
        , span(class = "h3", login$user)
        , p("(", login$role, ")")
        , tags$small(class = "text-muted", tolower(login$email))
        , actionLink("logout", "Logout")
        )
    )
  })
}