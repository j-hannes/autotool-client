Data model
==========

* Core
  * TaskType:                     Name,            Documentation, ExampleConfig,   _TaskConfigs_
  * TaskConfig:   _TaskType_,     Content,                                         _Assignments_
  * Assignment;   _TaskConfig_,   SubmissionTime,                                  _TaskInstances_
  * TaskInstance: _Assignment_,   TaskDescription, Documentation, ExampleSolution, _Solutions_
  * Solution:     _TaskInstance_, Content,         Score

* User
  * Tutor:   Name, Id, _TaskConfigs_, _Courses_
  * Student: Name, Id, _Courses_,     _TaskInstances_

* Organisation
  * Course: _Tutor_,  Name, Semester, _Assignments_, _Students_


Relationships
=============

TaskType-TaskConfig     1:n
TaskConfig-Assignment   1:n
Assignment-TaskInstance 1:n
TaskInstance-Solution   1:n

Tutor-TaskConfig     1:n
Tutor-Course         1:n (later possibly n:m)
Course-Assignment    1:n
Course-Student       n:m
Student-TaskInstance 1:n


Diagram
=======

                           +--------------+       +--------------+       +--------------+
                           |    Tutor     |------>|    Course    |<----->|   Student    |
                           +--------------+       +--------------+       +--------------+
                                  |                      |                      |
                                  |                      |                      |
                                  v                      v                      v
    +--------------+       +--------------+       +--------------+       +--------------+       +--------------+
    |   TaskType   |------>|  TaskConfig  |------>|  Assignment  |------>| TaskInstance |------>|   Solution   |
    +--------------+       +--------------+       +--------------+       +--------------+       +--------------+


Problems
========

When each entity represents an own data type, these cannot be put into separate
haskell modules, since all data types with a relation to another data type have
a reference to each other and need to import the other module which leads to a
"import cycle" which is not accepted by the compiler.

    module A (A) where
    import B (B)
    data A = A { a :: Int, b :: [B] }

    module B (B) where
    import A (A)
    data B = B { a :: String, b :: A }

We can see that module A contains a data type A, with a 1:n relationship to
data type B. Since it contains an attribute of type B it needs to import the
data type B from module B. This data type on the other hand belongs to a data
type A and needs to import this one from module A. But the Haskell compiler
does not allow such an import cycle which requires a different design or
configuration at this point.

One option would be to separate relations from the actual data. The result
could be something like:

    module A (A) where
    data A = A { a :: Int}

    module B (B) where
    data B = B { a :: String}

    module AB where
    data AB = AB { a :: A, bs :: [B]}

  - Probleme in funktionalen Programmiersprachen
    + Aktualisierung aller beteiligten DTs
    + cycle imports
    + Abstrahierung von Speicherobjekten (id, modified (+from), created, ...)
  - Aufbau vom natuerlichen Ansatz dauert zu lange -> schriftlich erlautern
  - Verwendung bestehender Bibliotheken (Lenses = getter, setter)
