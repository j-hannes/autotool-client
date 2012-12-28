Feature: Access landing page
  
  The application is run as a haskell web server and provides a landing page
  which is callable via a HTTP request.

  Scenario Outline: Everything is working smoothly

    The server process is running and can be reached from the command line via
    the program cURL.

    Given the server is up and running on <host>:<port>
     When I run the command curl <host>:<port>
     Then I get the response header "200 OK"
      And I get the body "autotool landing page" 

  Examples:
      | host             | port |
      | http://localhost | 8000 |
