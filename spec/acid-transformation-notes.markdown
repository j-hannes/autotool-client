# Weitere Notitzen

Eine Sammlung wesentlicher Punkte die für die Masterarbeit wichtig sind.

## Welche Rolle spielen Datenbanken für eine Webanwendung?

1. eine Webanwendung ist "zustandslos"

    Anders als eine Desktopanwendung, die normalerweise gestarted wird und dann
    durch den Benutzer von einem Zustand in den nächsten übergeführt wird, zum
    Beispiel "Willkommensbildschirm -> Menüauswahl -> Laden eines Dokumentes",
    und bei der das Programm über die gesamte Nutzung "läuft" und somit
    allemöglichen Benutzereingaben etc. im Speicher gehalten werden können...
    (bitte zwei Sätze daraus machen).

einfachste möglichkeit: IORef

## Datenentwurf

Wie sollten ein Datensystem entworfen werden, um den hier gestellten
Anforderungen gerecht zu werden? Sollten Identifikationsnummer sowie Beziehung
zwischen den Daten ausgelagert werden oder mit einbezogen werden?

# Transformation to ACID-State

## What's ACID-State?

- NoSQL Datenbank
- keine Datenumwandlungen erorderlich, Haskell-Datentypen können direkt
  gespeichert werden
- keine spezielle Abfragesprache erforderlich
