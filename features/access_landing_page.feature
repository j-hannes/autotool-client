Feature: Access landing page
  
  The application is run as a haskell web server and provides a landing page
  which is callable via a HTTP request.

  Scenario Outline: Everything is working smoothly
    Given the server is up and running on <url>:<port>
     When I curl <url>:<port>
     Then I get the response header "200 OK"
      And I get the body "autotool landing page" 

  Examples:
      | url              | port |
      | http://localhost | 8000 |
