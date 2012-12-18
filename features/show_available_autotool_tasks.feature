Feature: Show available autotool tasks

  To create an exercise one of the available tasks from the autotool must be selected.
  Therefore a list of all availbale autotool tasks must be shown where one of these can be
  selcted.

  Scenario: Connection to the autotool can be established
    Given I am on the main page of the application
    When I click on "new task"
    Then I get to a page that shows a list of all available tasks
