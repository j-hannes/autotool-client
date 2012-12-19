Synopsis
========

Der Kern der Anwendung ist das Erstellen von seminarbegleitenden
Uebungsaufgaben, welche dann von Studenten geloest werden sollen.

_Hintergrund ueber das Funktionieren von Autotool-Aufgaben (Konfiguration,
Instanziierung) sollte hier bereits vermittelt sein._

* Aufgaben = vom Autotool
* Uebungsaufgaben = konfigurierte Aufgaben


Entwicklungsprozess
===================

Die Software wird in folgenden Schritten entwickelt:

Die Planung der Software erfolgt Schritt fuer Schritt in Iterationen. Fuer jede
Iteration wird ein Feature umgesetzt, was in mehreren Tasks erfolgen kann.


Iteration 1
-----------

Erstellen der Basis-Infrastruktur (Server-Prozess)

Die Anwendung wird als Snap-Webanwendung umgesetzt, welche einen HTTP-Webserver
umfasst. Dieser kann gestartet werden und ist per HTTP erreichbar. Ein
GET-Anfrage auf eine entsprechende URL (Root-Domain) liefert eine fehlerfreie
Antwort (200 OK) sowie einen kurzen, textuellen Inhalt (noch kein HTML) der
Startseite der Anwendung.

### Features ###

* access langing page


Iteration 2
-----------

*Laden statischer Inhalte, Einbinden des Bootstrap-Frameworks*

Die Bootstrap CSS- und JavaScript-Bibliothek (von Twitter) ist in die Startseite
eingebunden. Der Inhalt der Startseite ist in HTML verfasst und optisch
gestaltet (minimaler Aufwand).

### Features ###

* display bootstrap style elements


Iteration 3
-----------

*Liste verfuegbarer Autotool-Aufgaben*

Eine Uebungsaufgabe wird immer aus einer Vorlage einer Aufgabe vom Autotool
erstellt. Dazu muss zunaechst eine Liste aller verfuegbaren Autotool-Aufgaben
angezeigt werden (diese soll immer aktuell vom Backen-Server abgefragt werden).

Diese Liste wird (nur) im Prozess der Erstellung einer neuen Uebungsaufgabe
benoetigt und soll daher durch einen entsprechenden Link auf der Startseite
erreichbar sein.

Die Liste soll ausserdem entsprechend der Aufgabenkategorien gegliedert sein.

### Features ###

* show available autotool tasks


Iteration 4
-----------

*Konfiguration einer Aufgabe*

Jede Aufgabe der Liste (aus Iteration 3) ist auf eine entsprechende Detailseite
der Aufgabe verlinkt, in welcher eine Beispielkonfiguration sowie relevante
Dokumentation einsehbar sind. Die Beispielkonfiguration kann veraendert werden.
Ein Name fuer die Uebungsaufgabe kann eingegeben werden.

Vor dem 'Speichern' der Uebungsaufgabe wird diese verifiziert, d.h. es wird
geprueft, ob die Grammatik der Konfiguration korrekt ist. Ist dies der Fall wird
die Uebungsaufgabe gespeichert. Ist die Konfiguration fehlerhaft wird die
Aufgabe nicht gespeichert und ein Warnhinweis angezeigt, welcher die
Fehlermeldung vom Backend Server enthaelt.


Iteration x
-----------

* Loesen von Aufgaben

* Benutzerauthentifizierung (Tutoren und Studenten)

* Kurssystem (Einschreibung, Zuordnen von Aufgaben)

* Speichern von Aufgabeninstanzen

* Speichern von Loesungen

* ...
