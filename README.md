Synopsis
========

Zweck der Anwendung ist das Erstellen von seminarbegleitenden
Uebungsaufgaben, welche dann von Studenten geloest werden sollen.

_Hintergrund ueber das Funktionieren von Autotool-Aufgaben (Konfiguration,
Instanziierung) sollte hier bereits vermittelt sein, ebenso der Hintergrund und
die Kernfunktionalitaeten._

Glossar
-------

* Aufgaben = vom Autotool
* Uebungsaufgaben = konfigurierte Aufgaben
* Assignment = gestellte Uebungsaufgaben (Kurs, Abgabetermin)
* Instanz = personalisierte Assignment
* Loesung = Loesung einer Instanz


Entwicklungsprozess
===================

Da die Software von nur einer Person erstellt wird, die Spezifikation, Entwurf
sowie Implementierung allein umsetzt, eignet sich hier eine agile
Entwicklungsmethode. Dokumentation kann dabei eher knapp ausfallen, aber
dennoch vollstaendig, da das Projekt im Rahmen einer Masterarbeit stattfindet
und darin in einem eigenen Kapitel erlaeutert wird.

Die Entwicklung erfolgt in Iterationen, wobei mit einer moeglichst einfachen
Variante der Software begonnen wird (minimale Funktionalitaet) und dann in
jeder Iteration eine weitere Funktionalitaet hinzugefuegt wird.

Zum Beispiel wird in der ersten Iteration nur das Erstellen von Uebungsaufgaben
umgesetzt. Damit ist der eigentliche Zweck der Anwendung zwar noch nicht
erfuellt, aber die Anwendung bietet schon eine Funktionalitaet, welche
eigenstaendig genutzt werden kann. Die naechste Iterationbeihaltet dann das
Loesen der Aufgaben, eine naechste koennte dann das Authentifizieren von
Nutzern beinhalten, eine weitere das Kurssystem, usw.

Diese Methode nennt man agil, da versucht wird, zu jedem Zeitpunkt eine
lauffaehige Version verfuegbar zu haben, die dann Schritt fuer Schritt
erweitert werden kann. Ausserdem koennen einzelne Iterationen auch kurzfristig
gewonnenen Erkenntnisse bzw. Aenderungen des urspruenglichen Plans beinhalten.
Damit kann der Entwicklungsprozess flexibler gestaltet, und auf notwendige
Aenderungen 'agiler' reagiert werden.

Jede Iteration wird wiederum in einzelne Tasks unterteilt, welche selbst eine
Erweiterung der Anwendung darstellen, die einen kleinen Bestandteil an
Funktionalitaet (Feature) hinzufuegen, wobei die Anwendung vor und nach jedem
Task trotzdem noch lauffaehig bleibt.


Iteration 1
-----------

_TODO: Iterationen ueberarbeiten, zum Beispiel Iteration 1 und 2
zusammenfassen, weitere Iterationen ebenfalls aktualisieren (wie oben
beschrieben). -> Dabei die Iterationen in Tasks unterteilen._

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
