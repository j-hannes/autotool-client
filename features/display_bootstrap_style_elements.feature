Feature: Display bootstrap style elements

  # This is not a business requirement., but rather a technology question.

  For quick prototyping we want to use the Bootstrap CSS framework from Twitter.
  Therefore the according CSS and JavaScript files have to be includes as static
  ressources in every served page.

  Scenario: Internet connection available
    Given I am connected to the internet
     When I am accessing the landing page of the application
     Then I can see a smoothly formatted button of the Bootstrap framework

  Scenario: Internet connection not available
    Given I am not connected to the internet
     When I am accessing the landing page of the application
     Then I can still see a smoothly formatted button of the Bootstrap framework
