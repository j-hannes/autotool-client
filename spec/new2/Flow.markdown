-------------------
   -- C O R E --
-------------------

soweit erledigt:

  [ ] Thematische Einleitung
  [X] Terminologie
  [X] Lastenheft
  [X] Pflichtenheft
  [.] Erfassung beteiligter Objekte und Operationen
  [X] Erstellung des ERDs
  [X] Erstellung der Datenstrukturen
  [.] Implementierung
  [ ] Beschreibung der Implementierung
  [ ] Untersuchung verschiedene Datenbankanbindungen


Vorgeschichte
=============

Foo bla wie wichtig ist es automatisierte Korrekturen von Hausaufgaben zu
haben. (Schonmal beschrieben - mal nachschauen!)


Terminologie
============

*Kurs* eine Abbildung einer Vorlesung / eines Moduls einer Universität.
*Übungsgruppe* zu jedem Kurs gehört meist ein oder mehrere Seminargruppe(n)
deren Aufnahmekapazität von Studenten beschränkt ist.

*Tutor* Leiter eines Kurses, erstellt Belege

*Student* Teilnehmer eines Kurses, löst Belege

*Einschreibung* bezeichnet hier die Registrierung eines Studenten mit einem
Kurs aus dem System, nicht eine Einschreibung im Rahmen der Kurswahl der
Universität

*Aufgabenvorlage* auch Aufgabentyp, ist eine Aufgabenstellung eines bestimmten
Themas (z.B. Bin-Packing), welche noch weiter konfiguriert werden kann

*Aufgabenkonfiguration* ist eine parametrisierte/konfigurierte Aufgabenvorlage
die gelöst werden kann

*Beleg* ist eine zu einem Kurs zugewiesene Aufgabenkonfiguration mit einem
festgelegtem Abgabezeitraum

*Aufgabeninstanz* ist eine durch einen Seed (meist Matrikelnummer)
individualisierte Aufgabenkonfiguration, d.h. jeder Student erhält eine leicht
abgeänderte Aufgabenstellung

*Lösung* ist eine eingesendete Lösung einer Aufgabeninstanz. Diese kann
entweder richtig oder falsch sein.


Lastenheft (Benutzersicht)
==========================

Die Software muss die folgenden Funktionen durchführen können:

1. Kurssystem
   1. Erstellen von Kursen (durch T) und festlegen bei wie viel Prozent
      gelöster Belege die Zulassung erhalten wird
   2. Erstellung von Übungsgruppen zu jedem Kurs (durch T)
   3. Einschreibung von Studenten in Übungsgruppe, falls noch Platz

2. Aufgabensystem
   1. Konfiguration von Aufgaben (durch T)
   2. Zuweisung von Aufgaben zu einem Kurs als Belegaufgabe mit Abgabezeitraum
      (durch T)
   3. Erhalten von Aufgaben eines Kurses, falls in entsprechend zugehörende
      Übungsgruppe eingeschrieben und falls Beginn des Abgabezeitraumes
      erreicht
   4. Lösen von erhaltenen Aufgaben. Punkte werden nur innerhalbs des
      Abgabezeitraumes erhalten, Aufgaben können jedoch noch weiterhin gelöst
      werden.


Pflichtenheft (Entwicklersicht)
===============================

Benutzertypen
-------------

Es gibt vier Aktuere in der Software:
* Tutoren (T)
* Studenten (S)
* System (Y)
* Administrator (A)

Tutoren müssen in der Lage sein, Kurse und Übungsgruppen sowie Aufgaben-
konfigurationen und Belege zu verwalten. Sie müssen ausserdem eine Übersicht
ihrer Kurse mit allen erstellen (laufenden und abgelaufenen) Belegen einsehen
können. Optional können verschiedene Attribute wie beste Bewertung eines
Beleges, Prozentsatz bestandener Studenten je Beleg und möglicherweise eine
Highscore-Tabelle zu jedem Kurs angezeigt werden.

Studenten müssen in der Lage sein, sich in aktuelle Kurse einzuschreiben und
darurch erhaltene Belegaufgaben zu lösen. Sie sollen ausserdem eine Übersicht
ihrer Belege zu jedem Kurs sowie ihren derzeitigen Zulassungsstatus einsehen
können. Optional können zusätzliche Daten sowie eine Highscore-Tabelle zu jedem
Kurs angezeit werden.

Die Differenzierung der zwei Hauptaktuere mit unterschiedlichen Rechten
erfordert eine Benutzerauthentifizierung, typischerweise durch Login und
Passwort. Dazu gehören Funktionen zum (automatischen) Vergeben und Zurücksetzen
von Logindaten, idealerweise über eine E-Mail-Adresse.

Darüber hinaus übernimmt das System automatische Prozeduren, wie das Errechnen
von Aufgabeninstanzen oder das Registrieren neuer Studenten.

Ein Administrator ist erforderlich, um direkt ins System einzugreifen, um zum
Beispiel neue Semester anzulegen oder Tutoren zu verwalten. Unter Umständen
sind dabei direkte Eingriffe in das Programm notwendig.

Kurssystem
----------

Kurse müssen von Tutoren verwaltet, dass heisst angelegt und geändert werden
können. Kurse zu denen noch keine Gruppe und kein Beleg zugewiesen ist, sollten
wieder gelöscht werden können. Kurse sind ausserdem einem Semester zugewiesen,
so dass nach Auswahl eines bestimmten Semesters nur Kurse dieses Semesters
angezeigt werden (siehe Kursübersicht).

Zu jedem Kurs gehört mindestens eine Übungsgruppe, in welche sich dann
Studenten einschreiben können, soweit der Einschreibezeitraum des Kurses
bereits begonnen, aber noch nocht abgelaufen und die Kapazität der Übungsgruppe
noch nicht erschöpft ist. Übungsgruppen können ebenfalls von Tutoren erstellt
und geändert werden. Ist noch kein Student in eine Übungsgruppe eingeschrieben,
so sollen diese auch noch gelöscht werden können.

Einschreibungen werden also von Studenten vorgenommen und können auch wieder
rückgängig gemacht werden, solange der Einschreibezeitraum noch offen ist.

Belegsystem
-----------

Mit Hilfe des Autotool-Backend-Servers (ABS) müssen Tutoren aus Aufgaben-
vorlagen konkrete (und valide) Aufgabenkonfigurationen erstellen können. Diese
können nicht mehr geändert, jedoch gelöscht werden, falls nicht weiter im
System verwendet. Aufgabenkonfigurationen stehen Tutoren über mehrere Semester
hinweg zur Verfügung.

Belege können daraufhin erstellt werden, indem eine Aufgabenkonfiguration zu
einem Kurs unter Angabe eines Abgabezeitraumes zugewiesen wird. Ist der Beginn
des Abgabezeitraumes erreicht, können alle Studenten, die in einer des Kurses
zugehörenden Übungsgruppe eingeschrieben sind, diesen Beleg sehen und lösen.
Nach Ablauf des Abgabezeitraumes ist der Beleg weiterhin sichtbar und lösbar,
allerdings gehen Lösungen dann nicht mehr in die Bewertung ein.

Damit Lösungen für Belege unter Studenten nicht einfach ausgetauscht werden
können, wird zu jedem Beleg für jeden Studenten eine Aufgabeninstanz erstellt,
dass heisst jeder Student erhält eine leicht abgeänderte Belegaufgaben, die
eine andere Lösung erfordert. Der Schwierigkeitsgrad bleibt hierbei jedoch
gleich. Gelöst werden also eigentlich nicht Belege, sondern Aufgabeninstanzen,
die zu einem Beleg gehören.

Die Anzahl eingesendeter Lösungen ist nicht beschränkt und hat keinen Einfluss
auf die Bewertung. Die Bewertung erfolgt durch den ABS, gemessen an der Größe
der eingesendeten Lösung sowie der Bewertungsart des Aufgabentyps (entweder je
größer desto besser oder je kleiner desto besser).


Objekte und Operationen
=======================

Tutor                 :: CR
Student               :: CR

Kurs                  :: CRU D*  *if nothing assigned
Übungsgruppe          :: CR UD*  *if nobody enrolled
Einschreibung         :: CR U*   *if enrollment time not over

Aufgabenvorlage       :: R
Aufgabenkonfiguration :: CR D*   *if not used
Beleg                 :: CRU D*  *if start time not reached
Aufgabeninstanz       :: CR
Lösung                :: CR


Implementierung
===============

Da das Erstellen der Software nicht Hauptgegenstand der Masterarbeit ist, wird
auf einen Teil der Funktionalität verzichtet. Darunter fällt das Löschen von
Objekten sowie das Bearbeiten bereits erstellter Objekte. Somit sind die
folgenden Funktionalitäten nicht gegeben:

* das Bearbeiten von angelegten Kursen
* das Löschen von Kursen, solange noch keine Gruppen oder Aufgaben zugewiesen
  sind
* das Bearbeiten oder Löschen von Übungsgruppen, solange noch keine
  Einschreibung erfolgt ist
* das Löschen erstellter Aufgabenkonfigurationen
* das Bearbeiten von Belegen
* das Löschen von Belegen, solange der Beginn der Bearbeitungszeit noch nicht
  erreicht ist

Die folgenden Objekte sind bereits im System vorhanden und können nicht durch
das Programm angelegt, bearbeitet oder gelöscht werden:

* Tutor, Studenten (jeweils im System)
* Aufgabenvorlage (im ABS vorhanden)

Die folgenden Objekte können durch einen Tutor erstellt werden:

* Kurs
* Übungsgruppe
* Aufgabenkonfiguration
* Beleg

Die folgenden Objekte werden durch das System erstellt:

* Aufgabeninstanz

Die folgenden Objekte können durch einen Student erstellt werden:

* Einschreibung
* Lösung


Abfrage von Objekten
====================

Neben der Abfrage einzelner Objekte über deren direkten Zugriff (durch z.B.
IDs) sind komplexere Abfragen über die Beziehungen bzw. Beziehungsketten
mehrerer Objekte hinweg erforderlich. 

Die folgenden Abfragen werden benötigt: (vgl. Abfragen.markdown)

Tutor
-----

* Liste aller Kurse (= Dashboard)
  - Einschreibung (startet/noch bis/vorbei seit)
  - Anzahl von Studenten, die den Kurs bestanden haben (%)
  - Gruppen-Tabelle
    - Bezeichnung
    - Kapazität
    - Eingeschrieben (Anz. Stud.)
  - Beleg-Tabelle
    - Name 
    - Aufgabentyp
    - Art (pflicht/optional)
    - Bearbeitungszeit (startet/noch bis/vorbei seit)
    - Einsendungen (davon erfolgreich)
    - Beste Bewertung
    - Anzahl von Studenten, die den Beleg erfolgreich gelöst haben (%)
  - Highscore-Tabelle (evtl)

* Liste aller Aufgabenvorlagen mit (= TaskTree)
  - Name

* Liste aller erstellten Aufgabenkonfigurationen mit (= Tasks)
  - Name
  - Aufgabentyp
  - Erstellungsdatum
  - Zugewiesen zu Kurs (ja/nein)
  - Link zum Lösen

System
------

* Details einer Aufgabenvorlage

Student
-------

* Liste aller eingeschriebenen Kurse  (= Dashboard)
  - Belege-Tabelle (deren Bearbeitungszeit bereits angefangen hat)
    - Bezeichnung
    - Art (pflicht/optional)
    - Bewertungsreihenfolde
    - Bearbeitungszeit (noch bis/vorbei seit)
    - vorige Bewertungen (ok{score,size}/no)
    - Gesamt-Wertungen (ok(n)/no(n))
    - Link zum Lösen
  - Highscore-Tabelle

* Liste aller noch nicht eingeschriebenen Kurse (= Einschreibungen)
  - Name des Kurses
  - Tutor
  - Einschreibung (startet/noch bis/vorbei seit)
  - Gruppen
    - Bezeichnung
    - Kapazität
    - Eingeschrieben (Anz. Stud.)
    - Link zum Einschreiben


Ideen für eine spätere Version
==============================

Tutoren sollen Studenten für Einschreibungen erst freischalten können.


Showing the list of Courses (for the tutor)
===========================================

listCourss


Calculating the Highscore
=========================

The the highscore is calculated
-------------------------------

The highscore is calculated for one specific course (in one semester). For each
assignment the best solution per student which has been submitted within the
submission time define the rank of the best students for this assignment. The
best solution is calculated by the score and the scoring order of the according
task. The best ranks then get points [20,15,10,7,6,5,4,3,2,1] according to
their positions. In the end the points of all students are summed up for all
assignments for each course.


Scenario
--------

MySolutionIntMap
  (1, Solution { ti=1, score=22, time="28.03.2013" ... })
  (2, Solution { ti=1, score=20, time="29.03.2013" ... })
  (3, Solution { ti=2, score=35, time="25.03.2013" ... })
  (4, Solution { ti=2, score=25, time="25.03.2013" ... })
  (5, Solution { ti=2, score=21, time="26.03.2013" ... })
  (6, Solution { ti=3, score=27, time="28.03.2013" ... })
  (7, Solution { ti=4, score=10, time="28.03.2013" ... })

MyTaskInstanceIntMap
  (1, TaskInstance { student=1, assignment=1 ... })
  (2, TaskInstance { student=2, assignment=1 ... })
  (3, TaskInstance { student=3, assignment=1 ... })
  (4, TaskInstance { student=1, assignment=2 ... })

MyStudentIntMap
  (1, Student { name="A", number"123" ... })
  (2, Student { name="B", number"125" ... })
  (3, Student { name="C", number"145" ... })
  (4, Student { name="D", number"144" ... })

MyAssignmentIntMap
  (1, Assignment { task=1, course=1, submission="25.03.2013-28.03.2013" ... })
  (2, Assignment { task=2, course=1, submission="28.03.2013-31.03.2013" ... })

MyTaskIntMap
  (1, Task {name="Hilbert-Quiz-23",   scoringOrder=Decreasing ... })
  (2, Task {name="Naive-Hillclimb-4", scoringOrder=Increasing ... })

MyCourseIntMap
  (1, Course { tutor=1, })


Quest
-----

Show the Highscore!

Student | Highscore
--------+----------
  12*   |    35
  1*5   |    20
  14*   |    10


Solution
--------


