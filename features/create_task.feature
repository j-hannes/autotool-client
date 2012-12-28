Feature: 

  In order to provide exercises to students
  As a tutor
  I want to create tasks

  Background:
    Given I am logged in as a Tutor

  Scenario: Create a first task
    Given I have no tasks in my task list
    When I create a new task
    Then I have one task in my task list

  Scenario Outline: Create a task with a specific name
    When I create a task with name <taskname>
    Then I have one task named <taskname> in my task list

  Examples:
    | taskname |
    | abcdefg  |
    | task23   |
