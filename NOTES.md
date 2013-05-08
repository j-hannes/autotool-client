Referenzierung von Datentypen untereinander

Beziehungsarten, Referenzierungsmethoden, Vor- und Nachteile:

Prinzipielle Methoden der Referenzierung
* Direkte Referenzierung => Objekte werden direkt als Referenz gespeichert
  Bsp.: 

* 1:1
  * Integration eines Datentyps als Attribut des anderen
    * (+) kurze wege
    * (+) Datenkonsistenz
    * (-) Kopplung / Abhaengigkeit
  * ein Datentyp enthaelt eine Referenz zum anderen Datentyp
    * (+) Unabhaengigkeit
    * (-) es muss eine Richtung bestimmt werden (referenziert A B oder B A)
  * beide Datentypen referenzieren sich gegenseitig
    * (+) schneller Datenzugriff
    * (+) lose Kopplung
    * (-) es muessen immer beide Objekte aktualisiert werden
* 1:n
  * Duplizierung des '1' Datentyps in jedem 'n' Datentyp
  * Integration des 'n' Datentyps in den '1' Datentyp
  * Referenzierung der 
* n:m
  * 

Bsp.:
-----


Disadvantages of file storage
=============================

* If the model changes all the data gets inconsistent
